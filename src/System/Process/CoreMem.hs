{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : System.Process.CoreMem
Copyright   : (c) 2023 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3

Provides functions that interpret the command line and memory scan of specified
processes
-}
module System.Process.CoreMem (
  getChoices,
  printProcs,
  fmtMem,
  fmtMemBytes,
) where

import Control.Exception (handle, throwIO)
import Control.Monad (filterM, unless, when)
import qualified Data.ByteString as BS
import Data.Char (isSpace)
import Data.Foldable (foldlM)
import Data.Hashable (hash)
import Data.List (foldl')
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.IO as Text
import qualified Data.Text.Read as Text
import Fmt (
  fixedF,
  padBothF,
  padLeftF,
  (+|),
  (+||),
  (|+),
  (|++|),
  (||+),
 )
import Numeric.Natural
import Options.Applicative
import Options.Applicative.NonEmpty (some1)
import System.Directory (
  doesFileExist,
  doesPathExist,
  getSymbolicLinkTarget,
  listDirectory,
 )
import System.Directory.Internal.Prelude (isDoesNotExistError, isPermissionError)
import System.FilePath (takeBaseName)
import System.Posix.Types (ProcessID)
import System.Posix.User (getEffectiveUserID)
import Text.Read (readEither, readMaybe)


printProcs :: (Choices, Target) -> IO ()
printProcs ct@(cs, target) = do
  let showSwap = choiceShowSwap cs
      print' (name, stats) = Text.putStrLn $ fmtCmdTotal showSwap name stats
      shouldShowTotal = (showSwap && tHasSwapPss target) || tHasPss target
      onlyTotal = choiceOnlyTotal cs
      reportSwapFlaw = Text.putStrLn . fmtSwapFlaw onlyTotal
      reportRamFlaw = Text.putStrLn . fmtRamFlaw onlyTotal
      printReport totals = do
        let overall = overallTotals $ Map.elems totals
        Text.putStrLn $ fmtAsHeader showSwap
        mapM_ print' $ Map.toList totals
        when shouldShowTotal $ Text.putStrLn $ fmtOverall showSwap overall

  foldlEitherM (readNameAndStats ct) (NE.toList $ tPids target) >>= \case
    Left err -> error $ show err
    Right xs | choiceByPid cs -> do
      let withPid (pid, name, stats) = ((pid, name), stats)
      printReport $ aggregate target $ map withPid xs
    Right xs -> do
      let dropId (_, name, stats) = (name, stats)
      printReport $ aggregate target $ map dropId xs

  -- when showSwap, report swap flaws
  -- unless (showSwap and onlyTotal), show ram flaws
  (ramFlaw, swapFlaw) <- checkForFlaws target
  when showSwap $ maybe (pure ()) reportSwapFlaw swapFlaw
  unless (onlyTotal && showSwap) $ maybe (pure ()) reportRamFlaw ramFlaw


readNameAndStats ::
  (Choices, Target) ->
  ProcessID ->
  IO (Either LostPid (ProcessID, Text, PerProc))
readNameAndStats (cs, target) pid = do
  let namer = if choiceSplitArgs cs then nameAsFullCmd else nameFor
  namer pid >>= \case
    Left e -> pure $ Left e
    Right name ->
      readMemStats target pid >>= \case
        Left e -> pure $ Left e
        Right stats -> pure $ Right (pid, name, stats)


-- result of getMemStats(Pid)
-- (Private, Shared, Shared_huge, Swap, mem_id)

-- result of parse_options
-- split_parses, pids_to_show, watch,   only_total, discriminate_by_pid, show_swap
--       Bool,    [Natural], Natural,       Bool,                Bool,      Bool
data Choices = Choices
  { choiceSplitArgs :: !Bool
  , choiceOnlyTotal :: !Bool
  , choiceByPid :: !Bool
  , choiceShowSwap :: !Bool
  , choiceWatchSecs :: !(Maybe Natural)
  , choicePidsToShow :: !(Maybe (NonEmpty ProcessID))
  }
  deriving (Eq, Show)


-- | Represents why the given pids are being scanned
data PidType = Requested | ViaRoot
  deriving (Eq, Show)


-- | Represents the information needed to perform the memory scan
data Target = Target
  { tPidType :: !PidType
  , tPids :: !(NonEmpty ProcessID)
  , tKernel :: !KernelVersion
  , tHasPss :: !Bool
  , tHasSwapPss :: !Bool
  , tHasSmaps :: !Bool
  }
  deriving (Eq, Show)


verify :: Choices -> IO (Choices, Target)
verify cs = case choicePidsToShow cs of
  Just tPids -> do
    -- halt if any specified pid cannot be accessed
    checkAllExist tPids
    target <- mkTarget Requested tPids
    pure (cs, target)
  Nothing -> do
    -- if choicePidsToShow is Nothing, must be running as root
    isRoot' <- isRoot
    unless isRoot' $ error "run as root if no pids given using -p"
    target <- allKnownProcs >>= mkTarget ViaRoot
    pure (cs, target)


mkTarget :: PidType -> NonEmpty ProcessID -> IO Target
mkTarget tPidType tPids = do
  tKernel <- readKernelVersion >>= either error pure
  (tHasSmaps, tHasPss, tHasSwapPss) <- confirmPss $ NE.head tPids
  pure Target {tPidType, tPids, tKernel, tHasPss, tHasSwapPss, tHasSmaps}


confirmPss :: ProcessID -> IO (Bool, Bool, Bool)
confirmPss pid = do
  let smapsPath = pidPath "smaps" pid
      containsPss = Text.isInfixOf "Pss:"
      containsSwapPss = Text.isInfixOf "SwapPss:"
      memtypes x = (True, containsPss x, containsSwapPss x)
  doesPathExist smapsPath >>= \case
    False -> pure (False, False, False)
    _ -> memtypes <$> readUtf8Text smapsPath


cmdInfo :: ParserInfo Choices
cmdInfo = info (helper <*> parseChoices) mempty


parseChoices :: Parser Choices
parseChoices =
  Choices
    <$> parseSplitArgs
    <*> parseOnlyTotal
    <*> parseDiscriminateByPid
    <*> parseShowSwap
    <*> optional parseWatchPeriodSecs
    <*> optional parseChoicesPidsToShow


parseChoicesPidsToShow :: Parser (NonEmpty ProcessID)
parseChoicesPidsToShow =
  some1 $
    option positiveNum $
      short 'p'
        <> long "pids"
        <> metavar "<pid1> [ -p pid2 ... -p pidN ]"
        <> help "Only show memory usage of the specified PIDs"


parseSplitArgs :: Parser Bool
parseSplitArgs =
  switch $
    short 's'
      <> long "split-args"
      <> help "Show and separate by all command line arguments"


parseOnlyTotal :: Parser Bool
parseOnlyTotal =
  switch $
    short 't'
      <> long "total"
      <> help "Only show the total value"


parseDiscriminateByPid :: Parser Bool
parseDiscriminateByPid =
  switch $
    short 'd'
      <> long "discriminate-by-pid"
      <> help "Show by process rather than by program"


parseShowSwap :: Parser Bool
parseShowSwap =
  switch $
    short 'S'
      <> long "show_swap"
      <> help "Show swap information"


parseWatchPeriodSecs :: Parser Natural
parseWatchPeriodSecs =
  option positiveNum $
    short 'w'
      <> long "watch"
      <> metavar "N"
      <> help "Measure and show memory every N seconds (N > 0)"


positiveNum :: (Read a, Ord a, Num a) => ReadM a
positiveNum =
  let
    checkPositive i
      | i > 0 = pure i
      | otherwise = readerError "Value must be greater than 0"
   in
    auto >>= checkPositive


parseKernelVersion :: Text -> Either String KernelVersion
parseKernelVersion =
  let unrecognized = Left "unrecognized kernel version"
      dec' (Right (x, extra)) | Text.null extra = Right x
      dec' _ = unrecognized
      dec1st' (Right (x, _)) = Right x
      dec1st' _ = unrecognized

      dec = dec' . Text.decimal
      dec1st = dec1st' . Text.decimal
      fromSplit [x] = (,,) <$> dec x <*> pure 0 <*> pure 0
      fromSplit [x, y] = (,,) <$> dec x <*> dec1st y <*> pure 0
      fromSplit [x, y, z] = (,,) <$> dec x <*> dec y <*> dec1st z
      fromSplit _ = unrecognized
   in fromSplit . Text.split (== '.')


type KernelVersion = (Natural, Natural, Natural)


{- | on linux kernels before smaps became available, there was no reliable way to
determine how much of a processes memory was shared

http://lkml.org/lkml/2005/7/6/250
-}
unknownShared :: KernelVersion -> Bool
unknownShared k = k >= (2, 6, 1) && k <= (2, 6, 9)


readKernelVersion :: IO (Either String KernelVersion)
readKernelVersion = parseKernelVersion <$> Text.readFile kernelVersionPath


kernelVersionPath :: String
kernelVersionPath = "/proc/sys/kernel/osrelease"


procRoot :: String
procRoot = "/proc/"


pidPath :: String -> ProcessID -> String
pidPath base pid = "" +| procRoot |++| toInteger pid |+ "/" +| base |+ ""


isRoot :: IO Bool
isRoot = (== 0) <$> getEffectiveUserID


getChoices :: IO (Choices, Target)
getChoices = execParser cmdInfo >>= verify


{- |  pidExists returns false for any ProcessID that does not exist or cannot
be accessed
-}
pidExeExists :: ProcessID -> IO Bool
pidExeExists = fmap (either (const False) (const True)) . exeInfo


--- given exe and cmd args
--- if split_args join cmd args with spaces replace newlines with spaces return
--- does exe end with deleted
--- if exe ends with (deleted) cmdline[0] exists path =  cmdline[0] (updated)
--- if exe_only; return basename(path)

--  if cmdline is empty, there is something wrong, ignore the process
--  this should only happen if the exe does not link to anything, so should not
--  be encountered

-- if the cmd has more than 1 arg
-- assume that the 1st is the exe name
-- if it ma

nameAsFullCmd :: ProcessID -> IO (Either LostPid Text)
nameAsFullCmd pid = do
  readSplitCmdLine pid >>= \case
    [] -> pure $ Left $ NoCmdLine pid
    xs -> pure $ Right $ Text.intercalate " " xs


nameFromExeOnly :: ProcessID -> IO (Either LostPid Text)
nameFromExeOnly pid = do
  exeInfo pid >>= \case
    Right i | not $ eiDeleted i -> pure $ Right $ baseName $ eiOriginal i
    -- when the exe target ends with (deleted), the version of the exe used to
    -- invoke the process has been removed from the filesystem. Sometimes it has
    -- been updated; examining both the original target and the version in
    -- cmdline help determine what occurred
    Right ExeInfo {eiOriginal = orig} ->
      exists orig >>= \case
        True -> pure $ Right $ baseName $ "" +| orig |+ " [updated]"
        _ ->
          readSplitCmdLine pid >>= \case
            (x : _) -> do
              let addSuffix' b = x <> if b then " [updated]" else " [deleted]"
              Right . baseName . addSuffix' <$> exists x
            -- args should not be empty when {pid_root}/exe resolves to a
            -- path, it's an error it is
            [] -> pure $ Left $ NoCmdLine pid
    Left e -> pure $ Left e


nameFor :: ProcessID -> IO (Either LostPid Text)
nameFor pid = do
  nameFromExeOnly pid >>= \case
    Left err -> pure $ Left err
    Right n -> parentNameIfMatched n pid


parentNameIfMatched :: Text -> ProcessID -> IO (Either LostPid Text)
parentNameIfMatched candidate pid = do
  let isMatch = flip Text.isPrefixOf candidate . siName
  statusInfo pid >>= \case
    Left err -> pure $ Left err
    Right si | isMatch si -> pure $ Right candidate
    Right si ->
      nameFromExeOnly (siParent si) >>= \case
        Right n | n == candidate -> pure $ Right n
        _ -> pure $ Right $ siName si


data ExeInfo = ExeInfo
  { eiTarget :: !Text
  -- ^ the path that the link /proc/<pid>/exe resolves to
  , eiOriginal :: !Text
  -- ^ a sanitized form of eiTarget; it removes the (deleted) suffix
  , eiDeleted :: !Bool
  -- ^ does eiTarget end with (deleted)?
  }
  deriving (Eq, Show)


data LostPid
  = NoExeFile ProcessID
  | NoStatusCmd ProcessID
  | NoStatusParent ProcessID
  | NoCmdLine ProcessID
  | BadStatm ProcessID
  | NoStatm ProcessID
  deriving (Eq, Show)


exeInfo :: ProcessID -> IO (Either LostPid ExeInfo)
exeInfo pid = do
  let rawPath = pidPath "exe" pid
      handledErr e = isDoesNotExistError e || isPermissionError e
      onIOE e = if handledErr e then pure (Left $ NoExeFile pid) else throwIO e
  handle onIOE $ do
    eiTarget <- takeTillNull . Text.pack <$> getSymbolicLinkTarget rawPath
    let eiDeleted = delEnd `Text.isSuffixOf` eiTarget
        withoutDeleted = Text.replace delEnd "" eiTarget
        eiOriginal = if eiDeleted then withoutDeleted else eiTarget
    pure $ Right $ ExeInfo {eiDeleted, eiOriginal, eiTarget}


delEnd :: Text
delEnd = " (deleted)"


data StatusInfo = StatusInfo
  { siName :: !Text
  , siParent :: !ProcessID
  }
  deriving (Eq, Show)


exists :: Text -> IO Bool
exists = doesPathExist . Text.unpack


readUtf8Text :: FilePath -> IO Text
readUtf8Text = fmap decodeUtf8 . BS.readFile


statusInfo :: ProcessID -> IO (Either LostPid StatusInfo)
statusInfo pid = do
  let rawPath = pidPath "status" pid
  fromLines pid . Text.lines <$> readUtf8Text rawPath


fromLines :: ProcessID -> [Text] -> Either LostPid StatusInfo
fromLines pid statusLines =
  let
    parseLine key l = Text.strip <$> Text.stripPrefix (key <> ":") l
    mkStep prefix acc l = case acc of
      Nothing -> parseLine prefix l
      found -> found
    name = maybe (Left $ NoStatusCmd pid) Right name'
    name' = foldl' (mkStep "Name") Nothing statusLines
    ppidTxt = foldl' (mkStep "PPid") Nothing statusLines
    parsePpid = readMaybe . Text.unpack
    ppId = maybe (Left $ NoStatusParent pid) Right (ppidTxt >>= parsePpid)
   in
    StatusInfo <$> name <*> ppId


readCmdLine :: ProcessID -> IO Text
readCmdLine = readUtf8Text . pidPath "cmdline"


readSplitCmdLine :: ProcessID -> IO [Text]
readSplitCmdLine = fmap (Text.split isNullOrSpace . Text.strip . dropEndNulls) . readCmdLine


nonExisting :: NonEmpty ProcessID -> IO [ProcessID]
nonExisting = filterM (fmap not . pidExeExists) . NE.toList


checkAllExist :: NonEmpty ProcessID -> IO ()
checkAllExist pids =
  nonExisting pids >>= \case
    [] -> pure ()
    xs -> error $ "halted: these PIDs cannot be found " ++ show xs


allKnownProcs :: IO (NonEmpty ProcessID)
allKnownProcs =
  let readNaturals = fmap (mapMaybe readMaybe)
      orNoPids = flip maybe pure $ error "did not find any process IDs"
   in readNaturals (listDirectory procRoot)
        >>= filterM pidExeExists
        >>= orNoPids . nonEmpty


baseName :: Text -> Text
baseName = Text.pack . takeBaseName . Text.unpack


isNull :: Char -> Bool
isNull = (== '\0')


isNullOrSpace :: Char -> Bool
isNullOrSpace x = isSpace x || isNull x


dropEndNulls :: Text -> Text
dropEndNulls = Text.dropWhileEnd isNull


takeTillNull :: Text -> Text
takeTillNull = Text.takeWhile (not . isNull)


readMemStats :: Target -> ProcessID -> IO (Either LostPid PerProc)
readMemStats target pid = do
  statmExists <- doesFileExist $ pidPath "statm" pid
  if
      | tHasSmaps target -> Right . fromSmap <$> readSmapStats target pid
      | statmExists -> readFromStatm (tKernel target) pid
      | otherwise -> pure $ Left $ NoStatm pid


readFromStatm :: KernelVersion -> ProcessID -> IO (Either LostPid PerProc)
readFromStatm version pid = do
  let noShared = unknownShared version
      mkStat rss _shared
        | noShared =
            ppZero
              { ppPrivate = rss * pageSizeKiB
              , ppMemId = fromInteger $ toInteger pid
              }
      mkStat rss shared =
        ppZero
          { ppShared = shared * pageSizeKiB
          , ppPrivate = (rss - shared) * pageSizeKiB
          , ppMemId = fromInteger $ toInteger pid
          }
  statm <- readUtf8Text $ pidPath "statm" pid
  case parseStatm $ Text.words statm of
    Right (_size : rss : shared : _xs) -> pure $ Right $ mkStat rss shared
    Right _ -> pure $ Left $ BadStatm pid
    Left _ -> pure $ Left $ BadStatm pid


-- value used as page size when @MemStat@ is calcuated from statm
pageSizeKiB :: Int
pageSizeKiB = 4


parseStatm :: [Text] -> Either String [Int]
parseStatm txts =
  let
    step (Right acc) txt = ((\x -> Right (x : acc)) =<< readEither (Text.unpack txt))
    step err _ = err
   in
    foldl' step (Right []) txts


-- | Represents the memory totals for an individual process
data PerProc = PerProc
  { ppPrivate :: !Int
  , ppShared :: !Int
  , ppSharedHuge :: !Int
  , ppSwap :: !Int
  , ppMemId :: !Int
  }
  deriving (Eq, Show)


ppZero :: PerProc
ppZero =
  PerProc
    { ppPrivate = 0
    , ppShared = 0
    , ppSharedHuge = 0
    , ppSwap = 0
    , ppMemId = 0
    }


fromSmap :: SmapStats -> PerProc
fromSmap ss =
  let pssTweak = ssPssCount ss `div` 2 -- add ~0.5 per line counter truncation
      pssShared = ssPss ss + pssTweak - ssPrivate ss
   in PerProc
        { ppSwap = if ssHasSwapPss ss then ssSwapPss ss else ssSwap ss
        , ppShared = if ssHasPss ss then pssShared else ssShared ss
        , ppSharedHuge = ssSharedHuge ss
        , ppPrivate = ssPrivate ss + ssPrivateHuge ss
        , ppMemId = ssMemId ss
        }


-- | Represents per-process data from  @PROC_ROOT@/smaps
data SmapStats = SmapStats
  { ssPss :: !Int
  , ssPssCount :: !Int
  , ssSwap :: !Int
  , ssSwapPss :: !Int
  , ssPrivate :: !Int
  , ssPrivateHuge :: !Int
  , ssSharedHuge :: !Int
  , ssShared :: !Int
  , ssMemId :: !Int
  , ssHasPss :: !Bool
  , ssHasSwapPss :: !Bool
  }
  deriving (Eq, Show)


ssZero :: SmapStats
ssZero =
  SmapStats
    { ssPss = 0
    , ssPssCount = 0
    , ssSwap = 0
    , ssSwapPss = 0
    , ssPrivate = 0
    , ssPrivateHuge = 0
    , ssSharedHuge = 0
    , ssShared = 0
    , ssHasSwapPss = False
    , ssHasPss = False
    , ssMemId = 0
    }


readSmapStats :: Target -> ProcessID -> IO SmapStats
readSmapStats _target pid = do
  smapLines <- readSmaps pid
  let stats = foldl' incrSmapStats ssZero smapLines
  pure $ stats {ssMemId = hash smapLines}


readSmaps :: ProcessID -> IO [Text]
readSmaps pid = do
  let lines' = fmap Text.lines . readUtf8Text
      smapPath = pidPath "maps" pid
      rollupPath = pidPath "smaps_rollup" pid
  hasSmaps <- doesFileExist smapPath
  hasRollup <- doesFileExist rollupPath
  if
      | hasRollup -> lines' rollupPath
      | hasSmaps -> lines' smapPath
      | otherwise -> pure []


-- Q: is it worth the dependency to replace this with lens from a lens package ?
incrPss
  , incrSwap
  , incrSwapPss
  , incrPrivate
  , incrPrivateHuge
  , incrShared
  , incrSharedHuge ::
    SmapStats -> Maybe Int -> SmapStats
incrPss ms = maybe ms $ \n -> ms {ssPss = n + ssPss ms}
incrSwap ms = maybe ms $ \n -> ms {ssSwap = n + ssSwap ms}
incrSwapPss ms = maybe ms $ \n -> ms {ssSwapPss = n + ssSwapPss ms}
incrPrivate ms = maybe ms $ \n -> ms {ssPrivate = n + ssPrivate ms}
incrShared ms = maybe ms $ \n -> ms {ssShared = n + ssShared ms}
incrPrivateHuge ms = maybe ms $ \n -> ms {ssPrivateHuge = n + ssPrivateHuge ms}
incrSharedHuge ms = maybe ms $ \n -> ms {ssSharedHuge = n + ssSharedHuge ms}


incrSmapStats :: SmapStats -> Text -> SmapStats
incrSmapStats acc l =
  if
      | Text.isPrefixOf "Private_Hugetlb:" l -> incrPrivateHuge acc $ smapValMb l
      | Text.isPrefixOf "Shared_Hugetlb:" l -> incrSharedHuge acc $ smapValMb l
      | Text.isPrefixOf "Shared" l -> incrShared acc $ smapValMb l
      | Text.isPrefixOf "Private" l -> incrPrivate acc $ smapValMb l
      | Text.isPrefixOf "Pss:" l ->
          let acc' = acc {ssHasPss = True, ssPssCount = 1 + ssPssCount acc}
           in incrPss acc' $ smapValMb l
      | Text.isPrefixOf "Swap:" l -> incrSwap acc $ smapValMb l
      | Text.isPrefixOf "SwapPss:" l -> incrSwapPss (acc {ssHasSwapPss = True}) $ smapValMb l
      | otherwise -> acc


smapValMb :: Read a => Text -> Maybe a
smapValMb l =
  let memWords = Text.words l
      readVal (_ : x : _) = readMaybe $ Text.unpack x
      readVal _ = Nothing
   in readVal memWords


-- | Describes the accuracy of main memory calculation
data RamFlaw
  = -- | no shared mem is reported
    NoSharedMem
  | -- | some shared mem not reported
    SomeSharedMem
  | -- | accurate only considering each process in isolation
    ExactForIsolatedMem
  deriving (Eq, Show, Ord)


fmtRamFlaw :: Bool -> RamFlaw -> Text
fmtRamFlaw onlyTotal accuracy =
  let
    prefix = if onlyTotal then "error: " else "warning: "
   in
    case accuracy of
      NoSharedMem ->
        Text.unlines
          [ prefix <> "shared memory is not reported by this system."
          , "Values reported will be too large, and totals are not reported"
          ]
      SomeSharedMem ->
        Text.unlines
          [ prefix <> "shared memory is not reported accurately by this system."
          , "Values reported could be too large, and totals are not reported"
          ]
      ExactForIsolatedMem ->
        Text.unlines
          [ prefix <> "shared memory is slightly over-estimated by this system"
          , "for each program, so totals are not reported."
          ]


-- | Describes inaccuracies in the swap measurement
data SwapFlaw
  = -- | not available
    NoSwap
  | -- | accurate only considering each process in isolation
    ExactForIsolatedSwap
  deriving (Eq, Show, Ord)


fmtSwapFlaw :: Bool -> SwapFlaw -> Text
fmtSwapFlaw onlyTotal flaw =
  let
    prefix = if onlyTotal then "error: " else "warning: "
   in
    case flaw of
      NoSwap -> prefix <> "swap is not reported by this system."
      ExactForIsolatedSwap ->
        Text.unlines
          [ prefix <> "swap is over-estimated by this system"
          , "for each program, so totals are not reported."
          ]


type Flaws = (Maybe RamFlaw, Maybe SwapFlaw)


checkForFlaws :: Target -> IO Flaws
checkForFlaws target = do
  let pid = NE.head $ tPids target
      version = tKernel target
      hasShared = unknownShared version
  case version of
    (2, 4, _) -> do
      let memInfoPath = pidPath "meminfo" pid
          alt = (Just SomeSharedMem, Just NoSwap)
          best = (Just ExactForIsolatedMem, Just NoSwap)
          containsInact = Text.isInfixOf "Inact_"
          checkInact x = if containsInact x then best else alt
      doesFileExist memInfoPath >>= \case
        False -> pure alt
        _ -> checkInact <$> readUtf8Text memInfoPath
    (2, 6, _) -> do
      let withSmaps = if tHasPss target then best else alt
          alt = (Just ExactForIsolatedMem, Just ExactForIsolatedSwap)
          best = (Nothing, Just ExactForIsolatedSwap)
          withNoSmaps = Just $ if hasShared then SomeSharedMem else NoSharedMem
      pure $ if tHasSmaps target then withSmaps else (withNoSmaps, Just NoSwap)
    (major, _, _) | major > 2 && tHasSmaps target -> do
      let alt = (Nothing, Just ExactForIsolatedSwap)
          best = (Nothing, Nothing)
      pure $ if tHasSwapPss target then best else alt
    _ -> pure (Just ExactForIsolatedMem, Just NoSwap)


data SubTotal = SubTotal
  { stShared :: !Int
  , stSharedHuge :: !Int
  , stPrivate :: !Int
  , stCount :: !Int
  , stSwap :: !Int
  , stMemIds :: !(Set Int)
  }
  deriving (Eq, Show)


-- If a process is invoked with clone using flags CLONE_VM and not CLONE_THREAD
-- it will share the same memory space as it's parent; this needs to accounted
-- for
--
-- This is detected by computing the memId has the hash of lines for the proc
-- read from its smaps file.
threadsNotProcs :: SubTotal -> Bool
threadsNotProcs cs = Set.size (stMemIds cs) == 1 && stCount cs > 1


aggregateStep ::
  Ord a =>
  Target ->
  Map a SubTotal ->
  (a, PerProc) ->
  Map a SubTotal
aggregateStep target acc (cmd, mem) =
  let combinePrivate next prev | tHasPss target = next + prev
      combinePrivate next prev = max next prev
      nextSt =
        SubTotal
          { stShared = ppShared mem
          , stSharedHuge = ppSharedHuge mem
          , stCount = 1
          , stPrivate = ppPrivate mem
          , stSwap = ppSwap mem
          , stMemIds = Set.singleton $ ppMemId mem
          }
      update' next prev =
        prev
          { stShared = stShared next + stShared prev
          , stSharedHuge = max (stSharedHuge next) (stSharedHuge prev)
          , stPrivate = combinePrivate (stPrivate next) (stPrivate prev)
          , stCount = stCount next + stCount prev
          , stSwap = stSwap next + stSwap prev
          , stMemIds = Set.union (stMemIds next) (stMemIds prev)
          }
   in Map.insertWith update' cmd nextSt acc


-- | Represents the memory totals for each command
data CmdTotal = CmdTotal
  { ctShared :: !Int
  , ctPrivate :: !Int
  , ctCount :: !Int
  , ctSwap :: !Int
  }
  deriving (Eq, Show)


overallTotals :: [CmdTotal] -> (Int, Int)
overallTotals cts =
  let step (private, swap) ct = (private + ctPrivate ct, swap + ctSwap ct)
   in foldl' step (0, 0) cts


fmtOverall :: Bool -> (Int, Int) -> Text
fmtOverall showSwap (private, swap) =
  let
    rimLength = if showSwap then 46 else 36
    gapLength = 26
    top = Text.replicate rimLength "-"
    gap = Text.replicate gapLength " "
    bottom = Text.replicate rimLength "="
    padl = padLeftF columnWidth ' ' . fmtMem
    withSwap = "" +| gap |++| padl private |++| padl swap |+ ""
    noSwap = "" +| gap |++| padl private |+ ""
    out = if showSwap then withSwap else noSwap
   in
    Text.unlines [top, out, bottom]


aggregate ::
  (AsCmdName a, Ord a) =>
  Target ->
  [(a, PerProc)] ->
  Map a CmdTotal
aggregate target = Map.map (rollup target) . foldl' (aggregateStep target) mempty


rollup :: Target -> SubTotal -> CmdTotal
rollup target st =
  let areThreads = threadsNotProcs st
      hasPss = tHasPss target
      reducedPrivate = stPrivate st `div` stCount st
      reducedShared = stShared st `div` stCount st
      newPrivate = if areThreads then reducedPrivate else stPrivate st
      newShared = if areThreads && hasPss then reducedShared else stShared st
   in CmdTotal
        { ctShared = newShared + stSharedHuge st
        , ctPrivate = newPrivate + newShared + stSharedHuge st
        , ctSwap = stSwap st
        , ctCount = stCount st
        }


fmtCmdTotal :: AsCmdName a => Bool -> a -> CmdTotal -> Text
fmtCmdTotal showSwap name ct =
  let
    padl = padLeftF columnWidth ' ' . fmtMem
    private = padl $ ctPrivate ct - ctShared ct
    shared = padl $ ctShared ct
    all' = padl $ ctPrivate ct
    swap' = padl $ ctSwap ct
    name' = cmdWithCount name $ ctCount ct
    ram = "" +| private |+ " + " +| shared |+ " = " +| all' |+ ""
    label = "" +| name' |+ ""
   in
    if showSwap
      then ram <> ("" +| swap' |+ "\t") <> label
      else ram <> "\t" <> label


data Power = Ki | Mi | Gi | Ti deriving (Eq, Show, Ord, Enum, Bounded)


fmtMem :: Int -> Text
fmtMem = fmtMem' Ki . fromIntegral


fmtMem' :: Power -> Float -> Text
fmtMem' =
  let doFmt p x = "" +| fixedF 1 x |+ " " +|| p ||+ "B"
      go p x | p == maxBound = doFmt p x
      go p x | x > 1000 = fmtMem' (succ p) (x / 1024)
      go p x = doFmt p x
   in go


fmtMemBytes :: Int -> Text
fmtMemBytes x = "" +| x * 1024 |+ ""


hdrPrivate, hdrShared, hdrRamUsed, hdrSwapUsed, hdrProgram :: Text
hdrPrivate = "Private"
hdrShared = "Shared"
hdrRamUsed = "RAM Used"
hdrSwapUsed = "Swap Used"
hdrProgram = "Program"


fmtAsHeader :: Bool -> Text
fmtAsHeader showSwap =
  let
    padb = padBothF columnWidth ' '
    private = padb hdrPrivate
    shared = padb hdrShared
    all' = padb hdrRamUsed
    name' = padb hdrProgram
    swap' = padb hdrSwapUsed
    ram = "" +| private |+ " + " +| shared |+ " = " +| all' |+ ""
    label = "" +| name' |+ ""
   in
    if showSwap
      then ram <> ("" +| swap' |+ "\t") <> label
      else ram <> "\t" <> label


cmdWithCount :: AsCmdName a => a -> Int -> Text
cmdWithCount cmd count = "" +| asCmdName cmd |+ " (" +| count |+ ")"


-- | Represents a label that is as the cmd name in the report output
class AsCmdName a where
  -- Convert the data to text for output
  asCmdName :: a -> Text


instance AsCmdName Text where
  asCmdName = id


instance AsCmdName (ProcessID, Text) where
  asCmdName (pid, name) = "" +| name |+ " [" +| toInteger pid |+ "]"


foldlEitherM ::
  Monad m =>
  (a -> m (Either b c)) ->
  [a] ->
  m (Either b [c])
foldlEitherM f xs =
  let go (Left err) _ = pure $ Left err
      go (Right acc) a =
        f a >>= \case
          Left err -> pure $ Left err
          Right y -> pure $ Right (y : acc)
   in foldlM go (Right []) xs


columnWidth :: Int
columnWidth = 10
