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
) where

import Control.Exception (handle, throwIO)
import Control.Monad (filterM, unless)
import qualified Data.ByteString as BS
import Data.Char (isSpace)
import Data.Foldable (foldlM)
import Data.Hashable (hash)
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
import Fmt ((+|), (|+), (|++|))
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
  foldlEitherM (readNameAndStats ct) (NE.toList $ tPids target) >>= \case
    Left err -> error $ show err
    Right xs | choiceByPid cs -> do
      let withId (x, y, z) = (x, (x, y), z)
      mapM_ print $ Map.toList $ aggregate target $ map withId xs
    Right xs -> mapM_ print $ Map.toList $ aggregate target xs
  printAccuracy target


readNameAndStats ::
  (Choices, Target) ->
  ProcessID ->
  IO (Either LostPid (ProcessID, CmdName, MemStats))
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
        <> metavar "<pid>[ -p pid2 ... -p pidN ]"
        <> help "Only show memory usage of the specified PIDs"


parseSplitArgs :: Parser Bool
parseSplitArgs =
  switch $
    short 's'
      <> long "split-parses"
      <> help "Show and separate by, all command line parseuments"


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
    mkStep prefix l acc = case acc of
      Nothing -> parseLine prefix l
      found -> found
    name = maybe (Left $ NoStatusCmd pid) Right name'
    name' = foldr (mkStep "Name") Nothing statusLines
    ppidTxt = foldr (mkStep "PPid") Nothing statusLines
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


readMemStats :: Target -> ProcessID -> IO (Either LostPid MemStats)
readMemStats target pid = do
  statmExists <- doesFileExist $ pidPath "statm" pid
  if
      | tHasSmaps target -> Right . fromSmap <$> readSmapStats target pid
      | statmExists -> readFromStatm (tKernel target) pid
      | otherwise -> pure $ Left $ NoStatm pid


readFromStatm :: KernelVersion -> ProcessID -> IO (Either LostPid MemStats)
readFromStatm version pid = do
  let noShared = unknownShared version
      mkStat rss _shared
        | noShared =
            memZero
              { msPrivate = rss * pageSizeKiB
              , msMemId = fromInteger $ toInteger pid
              }
      mkStat rss shared =
        memZero
          { msShared = shared * pageSizeKiB
          , msPrivate = (rss - shared) * pageSizeKiB
          , msMemId = fromInteger $ toInteger pid
          }
  statm <- readUtf8Text $ pidPath "statm" pid
  case readInts $ Text.words statm of
    Right (_size : rss : shared : _xs) -> pure $ Right $ mkStat rss shared
    Right _ -> pure $ Left $ BadStatm pid
    Left _ -> pure $ Left $ BadStatm pid


-- value used as page size when @MemStat@ is calcuated from statm
pageSizeKiB :: Int
pageSizeKiB = 4


readInts :: [Text] -> Either String [Int]
readInts txts =
  let
    step txt (Right acc) = ((\x -> Right (x : acc)) =<< readEither (Text.unpack txt))
    step _ err = err
   in
    foldr step (Right []) txts


-- | Represents the memory summary for a process
data MemStats = MemStats
  { msPrivate :: !Int
  , msShared :: !Int
  , msSharedHuge :: !Int
  , msSwap :: !Int
  , msMemId :: !Int
  }
  deriving (Eq, Show)


memZero :: MemStats
memZero =
  MemStats
    { msPrivate = 0
    , msShared = 0
    , msSharedHuge = 0
    , msSwap = 0
    , msMemId = 0
    }


fromSmap :: SmapStats -> MemStats
fromSmap ss =
  let pssTweak = ssPssCount ss `div` 2 -- add ~0.5 per line counter truncation
      pssShared = ssPss ss + pssTweak - ssPrivate ss
   in MemStats
        { msSwap = if ssHasSwapPss ss then ssSwapPss ss else ssSwap ss
        , msShared = if ssHasPss ss then pssShared else ssShared ss
        , msSharedHuge = ssSharedHuge ss
        , msPrivate = ssPrivate ss + ssPrivateHuge ss
        , msMemId = ssMemId ss
        }


-- | Represents data accumulated by reading @PROC_ROOT@/smaps
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


smapZero :: SmapStats
smapZero =
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
  let stats = foldr incrMemStats smapZero smapLines
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


incrMemStats :: Text -> SmapStats -> SmapStats
incrMemStats l acc =
  if
      | Text.isPrefixOf "Private_Hugetlb:" l -> incrPrivateHuge acc $ memValMb l
      | Text.isPrefixOf "Shared_Hugetlb:" l -> incrSharedHuge acc $ memValMb l
      | Text.isPrefixOf "Shared" l -> incrShared acc $ memValMb l
      | Text.isPrefixOf "Private" l -> incrPrivate acc $ memValMb l
      | Text.isPrefixOf "Pss:" l ->
          let acc' = acc {ssHasPss = True, ssPssCount = 1 + ssPssCount acc}
           in incrPss acc' $ memValMb l
      | Text.isPrefixOf "Swap:" l -> incrSwap acc $ memValMb l
      | Text.isPrefixOf "SwapPss:" l -> incrSwapPss (acc {ssHasSwapPss = True}) $ memValMb l
      | otherwise -> acc


memValMb :: Read a => Text -> Maybe a
memValMb l =
  let memWords = Text.words l
      readVal (_ : x : _) = readMaybe $ Text.unpack x
      readVal _ = Nothing
   in readVal memWords


-- | Describes the accuracy of main memory calculation
data RamAccuracy
  = -- | no shared mem is reported
    NoSharedMem
  | -- | some shared mem not reported
    SomeSharedMem
  | -- | accurate only considering each process in isolation
    ExactForIsolatedMem
  | -- | accurate and can total
    ExactMem
  deriving (Eq, Show, Ord)


-- | Describes the accuracy of the swap measurement
data SwapAccuracy
  = -- | not available
    NoSwap
  | -- | accurate only considering each process in isolation
    ExactForIsolatedSwap
  | -- | accurate and can total
    ExactSwap
  deriving (Eq, Show, Ord)


type Accuracy = (RamAccuracy, SwapAccuracy)


data CmdStats = CmdStats
  { csShared :: !Int
  , csSharedHuge :: !Int
  , csPrivate :: !Int
  , csCount :: !Int
  , csSwap :: !Int
  , csMemIds :: !(Set Int)
  }
  deriving (Eq, Show)


type CmdName = Text


aggregate ::
  (Show a, Ord a) =>
  Target ->
  [(ProcessID, a, MemStats)] ->
  Map a CmdStats
aggregate target = foldr (aggregateStep target) mempty


aggregateStep ::
  Ord a =>
  Target ->
  (ProcessID, a, MemStats) ->
  Map a CmdStats ->
  Map a CmdStats
aggregateStep target (pid_, cmd, mem) acc =
  let combinePrivate next prev | tHasPss target = next + prev
      combinePrivate next prev = max next prev
      nextCs =
        CmdStats
          { csShared = msShared mem
          , csSharedHuge = msSharedHuge mem
          , csCount = 1
          , csPrivate = msPrivate mem
          , csSwap = msSwap mem
          , csMemIds = Set.singleton $ msMemId mem
          }
      update' next prev =
        prev
          { csShared = csShared next + csShared prev
          , csSharedHuge = max (csSharedHuge next) (csSharedHuge prev)
          , csPrivate = combinePrivate (csPrivate next) (csPrivate prev)
          , csCount = csCount next + csCount prev
          , csSwap = csSwap next + csSwap prev
          , csMemIds = Set.union (csMemIds next) (csMemIds prev)
          }
   in Map.insertWith update' cmd nextCs acc


rollup :: Target -> CmdStats -> CmdStats
rollup target cs =
  let areThreads = areThreadsNotProcs cs
      hasPss = tHasPss target
      reducedPrivate = csPrivate cs `div` csCount cs
      reducedShared = csShared cs `div` csCount cs
      newPrivate = if areThreads then reducedPrivate else csPrivate cs
      newShared = if areThreads && hasPss then reducedShared else csShared cs
   in cs
        { csShared = newShared + csSharedHuge cs
        , csPrivate = newPrivate + newShared + csSharedHuge cs
        }


-- If a process is invoked with clone using flags CLONE_VM and not CLONE_THREAD
-- it will share the same memory space as it's parent; this needs to accounted
-- for
--
-- This is detected by computing the memId has the hash of lines for the proc
-- read from its smaps file.
areThreadsNotProcs :: CmdStats -> Bool
areThreadsNotProcs cs = Set.size (csMemIds cs) == 1 && csCount cs > 1


printAccuracy :: Target -> IO ()
printAccuracy target = do
  (memAcc, swapAcc) <- readAccuracy target
  print (tKernel target, memAcc, swapAcc)


readAccuracy :: Target -> IO Accuracy
readAccuracy target = do
  let pid = NE.head $ tPids target
      version = tKernel target
      hasShared = unknownShared version
  case version of
    (2, 4, _) -> do
      let memInfoPath = pidPath "meminfo" pid
          alt = (SomeSharedMem, NoSwap)
          best = (ExactForIsolatedMem, NoSwap)
          containsInact = Text.isInfixOf "Inact_"
          checkInact x = if containsInact x then best else alt
      doesFileExist memInfoPath >>= \case
        False -> pure alt
        _ -> checkInact <$> readUtf8Text memInfoPath
    (2, 6, _) -> do
      let withSmaps = if tHasPss target then best else alt
          alt = (ExactForIsolatedMem, ExactForIsolatedSwap)
          best = (ExactMem, ExactForIsolatedSwap)
          withNoSmaps = if hasShared then SomeSharedMem else NoSharedMem
      pure $ if tHasSmaps target then withSmaps else (withNoSmaps, NoSwap)
    (major, _, _) | major > 2 && tHasSmaps target -> do
      let alt = (ExactMem, ExactForIsolatedSwap)
          best = (ExactMem, ExactSwap)
      pure $ if tHasSwapPss target then best else alt
    _ -> pure (ExactForIsolatedMem, NoSwap)


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
