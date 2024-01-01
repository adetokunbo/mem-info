{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : System.Process.CoreMem
Copyright   : (c) 2023 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3

Implements a command that computes the memory usage of processes specified via
the command's arguments
-}
module System.Process.CoreMem (
  getChoices,
  printProcs,
) where

import qualified Data.ByteString as BS
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
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
import Options.Applicative
import System.Directory (
  doesFileExist,
  doesPathExist,
  getSymbolicLinkTarget,
  listDirectory,
 )
import System.Exit (exitFailure)
import System.MemInfo.Choices (Choices (..), cmdInfo)
import System.MemInfo.Proc (
  CmdTotal (..),
  PerProc (..),
  amass,
  parseFromSmap,
  parseFromStatm,
 )
import System.MemInfo.SysInfo (KernelVersion, readKernelVersion, unknownShared)
import System.Posix.User (getEffectiveUserID)
import System.Process.CoreMem.Prelude


{- | Report on the memory usage of the processes given in @Target@

Structure the report output as indicated by @Choices@
-}
printProcs :: (Choices, Target) -> IO ()
printProcs ct@(cs, target) = do
  let showSwap = choiceShowSwap cs
      onlyTotal = choiceOnlyTotal cs
      overallIsAccurate = (showSwap && tHasSwapPss target) || tHasPss target
      print' (name, stats) = Text.putStrLn $ fmtCmdTotal showSwap name stats
      printRawTotal = Text.putStrLn . fmtMemBytes
      withPid (pid, name, stats) = ((pid, name), stats)
      dropId (_, name, stats) = (name, stats)
      printEachCmd totals = do
        let overall = overallTotals $ Map.elems totals
        Text.putStrLn $ fmtAsHeader showSwap
        mapM_ print' $ Map.toList totals
        when overallIsAccurate $ Text.putStrLn $ fmtOverall showSwap overall
        checkForFlaws target >>= reportFlaws showSwap onlyTotal
      printTheTotal totals = do
        let (private, swap) = overallTotals $ Map.elems totals
        flaws@(ram, sw) <- checkForFlaws target
        if showSwap
          then do
            when (tHasSwapPss target) $ printRawTotal swap
            reportFlaws showSwap onlyTotal flaws
            when (isJust sw) exitFailure
          else do
            when (tHasPss target) $ printRawTotal private
            reportFlaws showSwap onlyTotal flaws
            when (isJust ram) exitFailure
      cmdReport cmds = if onlyTotal then printTheTotal cmds else printEachCmd cmds

  if choiceByPid cs
    then case choiceWatchSecs cs of
      Nothing -> withCmdTotals ct cmdReport withPid
      Just period -> withCmdTotals' period ct cmdReport withPid
    else case choiceWatchSecs cs of
      Nothing -> withCmdTotals ct cmdReport dropId
      Just period -> withCmdTotals' period ct cmdReport dropId


withCmdTotals ::
  (Ord c, AsCmdName c) =>
  (Choices, Target) ->
  (Map c CmdTotal -> IO b) ->
  ((ProcessID, Text, PerProc) -> (c, PerProc)) ->
  IO b
withCmdTotals ct@(_, target) printCmds mkCmd = do
  foldlEitherM (readNameAndStats ct) (NE.toList $ tPids target) >>= \case
    Left err -> error $ show err
    Right cmds -> printCmds $ amass (tHasPss target) $ map mkCmd cmds


withCmdTotals' ::
  (Ord c, AsCmdName c) =>
  Natural ->
  (Choices, Target) ->
  (Map c CmdTotal -> IO ()) ->
  ((ProcessID, Text, PerProc) -> (c, PerProc)) ->
  IO ()
withCmdTotals' delaySecs ct@(_, target) printCmds mkCmd = do
  let periodMicros = 1000000 * fromInteger (toInteger delaySecs)
      clearScreen = putStrLn "\o033c"
      go =
        foldlEitherM' (readNameAndStats ct) (NE.toList $ tPids target) >>= \case
          (pids, []) -> do
            warnStopped pids
            Text.putStrLn "all monitored processes have stopped; terminating..."
          (pids, xs) -> do
            clearScreen
            unless (null pids) $ warnStopped pids
            printCmds $ amass (tHasPss target) $ map mkCmd xs
            threadDelay periodMicros
            go
  go


warnStopped :: [ProcessID] -> IO ()
warnStopped pids = do
  let errMsg = "some processes stopped and will no longer appear:pids:" +| toInteger <$> pids |+ ""
  errStrLn False errMsg


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


reportFlaws :: Bool -> Bool -> Flaws -> IO ()
reportFlaws showSwap onlyTotal (ramFlaw, swapFlaw) = do
  let reportSwapFlaw = errStrLn onlyTotal . fmtSwapFlaw
      reportRamFlaw = errStrLn onlyTotal . fmtRamFlaw
  -- when showSwap, report swap flaws
  -- unless (showSwap and onlyTotal), show ram flaws
  when showSwap $ maybe (pure ()) reportSwapFlaw swapFlaw
  unless (onlyTotal && showSwap) $ maybe (pure ()) reportRamFlaw ramFlaw


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


procRoot :: String
procRoot = "/proc/"


pidPath :: String -> ProcessID -> FilePath
pidPath base pid = "" +| procRoot |++| toInteger pid |+ "/" +| base |+ ""


isRoot :: IO Bool
isRoot = (== 0) <$> getEffectiveUserID


-- | Parse the command arguments and verify that the command can run.
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
  readCmdline pid >>= \case
    Nothing -> pure $ Left $ NoCmdLine pid
    Just xs -> pure $ Right $ Text.intercalate " " $ NE.toList xs


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
          readCmdline pid >>= \case
            Just (x :| _) -> do
              let addSuffix' b = x <> if b then " [updated]" else " [deleted]"
              Right . baseName . addSuffix' <$> exists x
            -- args should not be empty when {pid_root}/exe resolves to a
            -- path, it's an error if it is
            Nothing -> pure $ Left $ NoCmdLine pid
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
      takeTillNull = Text.takeWhile (not . isNull)
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


readCmdline :: ProcessID -> IO (Maybe (NonEmpty Text))
readCmdline = fmap toArgs . readUtf8Text . pidPath "cmdline"


toArgs :: Text -> Maybe (NonEmpty Text)
toArgs =
  let split' = Text.split isNullOrSpace . Text.strip . dropEndNulls
   in nonEmpty . split'


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


dropEndNulls :: Text -> Text
dropEndNulls = Text.dropWhileEnd isNull


readMemStats :: Target -> ProcessID -> IO (Either LostPid PerProc)
readMemStats target pid = do
  statmExists <- doesFileExist $ pidPath "statm" pid
  if
      | tHasSmaps target -> Right . parseFromSmap <$> readSmaps pid
      | statmExists -> do
          let readStatm' = readUtf8Text $ pidPath "statm" pid
              orLostPid = maybe (Left $ BadStatm pid) Right
          orLostPid . parseFromStatm (tKernel target) <$> readStatm'
      | otherwise -> pure $ Left $ NoStatm pid


readSmaps :: ProcessID -> IO Text
readSmaps pid = do
  let smapPath = pidPath "maps" pid
      rollupPath = pidPath "smaps_rollup" pid
  hasSmaps <- doesFileExist smapPath
  hasRollup <- doesFileExist rollupPath
  if
      | hasRollup -> readUtf8Text rollupPath
      | hasSmaps -> readUtf8Text smapPath
      | otherwise -> pure Text.empty


-- | Describes inaccuracies in the RAM calculation
data RamFlaw
  = -- | no shared mem is reported
    NoSharedMem
  | -- | some shared mem not reported
    SomeSharedMem
  | -- | accurate only considering each process in isolation
    ExactForIsolatedMem
  deriving (Eq, Show, Ord)


fmtRamFlaw :: RamFlaw -> Text
fmtRamFlaw NoSharedMem =
  Text.unlines
    [ "shared memory is not reported by this system."
    , "Values reported will be too large, and totals are not reported"
    ]
fmtRamFlaw SomeSharedMem =
  Text.unlines
    [ "shared memory is not reported accurately by this system."
    , "Values reported could be too large, and totals are not reported"
    ]
fmtRamFlaw ExactForIsolatedMem =
  Text.unlines
    [ "shared memory is slightly over-estimated by this system"
    , "for each program, so totals are not reported."
    ]


-- | Describes inaccuracies in the swap measurement
data SwapFlaw
  = -- | not available
    NoSwap
  | -- | accurate only considering each process in isolation
    ExactForIsolatedSwap
  deriving (Eq, Show, Ord)


fmtSwapFlaw :: SwapFlaw -> Text
fmtSwapFlaw NoSwap = "swap is not reported by this system."
fmtSwapFlaw ExactForIsolatedSwap =
  Text.unlines
    [ "swap is over-estimated by this system"
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


foldlEitherM' ::
  Monad m =>
  (a -> m (Either b c)) ->
  [a] ->
  m ([a], [c])
foldlEitherM' f xs =
  let
    go (as, cs) a =
      f a >>= \case
        Left _ -> pure (a : as, cs)
        Right c -> pure (as, c : cs)
   in
    foldlM go (mempty, mempty) xs


columnWidth :: Int
columnWidth = 10


errStrLn :: Bool -> Text -> IO ()
errStrLn errOrWarn txt = do
  let prefix = if errOrWarn then "error: " else "warning: "
  Text.hPutStrLn stderr $ prefix <> txt
