{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : System.MemInfo
Copyright   : (c) 2023 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3

Implements a command that computes the memory usage of some processes
-}
module System.MemInfo (
  -- * implement the system command @printmem@
  getChoices,
  printProcs,

  -- * read @MemUsage@ directly
  LostPid (..),
  NotRun (..),
  readMemUsage,
  readMemUsage',
  readForOnePid,

  -- * unfold @MemUsage@ in a stream
  unfoldMemUsageAfter',
  unfoldMemUsageAfter,
  unfoldMemUsage,

  -- * determine the process/program name
  nameFromExeOnly,
  nameFor,
  nameAsFullCmd,

  -- * index by program name or by processID
  ProcName,
  dropId,
  withPid,

  -- * re-export
  mkReportBud,
) where

import Data.Bifunctor (Bifunctor (..))
import Data.Functor ((<&>))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Fmt (
  listF,
  (+|),
  (|+),
  (|++|),
 )
import System.Exit (exitFailure)
import System.MemInfo.Choices (Choices (..), getChoices)
import System.MemInfo.Prelude
import System.MemInfo.Print (
  AsCmdName (..),
  fmtAsHeader,
  fmtMemUsage,
  fmtOverall,
 )
import System.MemInfo.Proc (
  BadStatus (..),
  ExeInfo (..),
  MemUsage (..),
  PerProc (..),
  StatusInfo (..),
  amass,
  parseExeInfo,
  parseFromSmap,
  parseFromStatm,
  parseStatusInfo,
 )
import System.MemInfo.SysInfo (
  ReportBud (..),
  fmtRamFlaws,
  fmtSwapFlaws,
  mkReportBud,
 )
import System.Posix.User (getEffectiveUserID)


{- | Print a report to @stdout@ displaying the memory usage of the programs
specified by @Choices@
-}
printProcs :: Choices -> IO ()
printProcs cs = do
  bud <- verify cs
  let Choices
        { choiceShowSwap = showSwap
        , choiceOnlyTotal = onlyTotal
        , choiceWatchSecs = watchSecsMb
        , choiceByPid = byPid
        } = cs
      printEachCmd totals = printMemUsages bud showSwap onlyTotal totals
      printTheTotal = onlyPrintTotal bud showSwap onlyTotal
      showTotal cmds = if onlyTotal then printTheTotal cmds else printEachCmd cmds
      namer = if choiceSplitArgs cs then nameAsFullCmd else nameFor
  case (watchSecsMb, byPid) of
    (Nothing, True) -> readMemUsage' namer withPid bud >>= either haltLostPid showTotal
    (Nothing, _) -> readMemUsage' namer dropId bud >>= either haltLostPid showTotal
    (Just spanSecs, True) -> do
      let unfold = unfoldMemUsageAfter' namer withPid spanSecs
      loopPrintMemUsages unfold bud showTotal
    (Just spanSecs, _) -> do
      let unfold = unfoldMemUsageAfter' namer dropId spanSecs
      loopPrintMemUsages unfold bud showTotal


printMemUsages :: AsCmdName a => ReportBud -> Bool -> Bool -> Map a MemUsage -> IO ()
printMemUsages bud showSwap onlyTotal totals = do
  let overall = overallTotals $ Map.elems totals
      overallIsAccurate = (showSwap && rbHasSwapPss bud) || rbHasPss bud
      print' (name, stats) = Text.putStrLn $ fmtMemUsage showSwap name stats
  Text.putStrLn $ fmtAsHeader showSwap
  mapM_ print' $ Map.toList totals
  when overallIsAccurate $ Text.putStrLn $ fmtOverall showSwap overall
  reportFlaws bud showSwap onlyTotal


onlyPrintTotal :: ReportBud -> Bool -> Bool -> Map k MemUsage -> IO ()
onlyPrintTotal bud showSwap onlyTotal totals = do
  let (private, swap) = overallTotals $ Map.elems totals
      printRawTotal = Text.putStrLn . fmtMemBytes
  if showSwap
    then do
      when (rbHasSwapPss bud) $ printRawTotal swap
      reportFlaws bud showSwap onlyTotal
      when (isJust $ rbSwapFlaws bud) exitFailure
    else do
      when (rbHasPss bud) $ printRawTotal private
      reportFlaws bud showSwap onlyTotal
      when (isJust $ rbRamFlaws bud) exitFailure


loopPrintMemUsages ::
  (Ord c, AsCmdName c) =>
  (ReportBud -> IO (Either [ProcessID] ((Map c MemUsage, [ProcessID]), ReportBud))) ->
  ReportBud ->
  (Map c MemUsage -> IO ()) ->
  IO ()
loopPrintMemUsages unfold bud showTotal = do
  let clearScreen = putStrLn "\o033c"
      warnHalting = errStrLn False "halting: all monitored processes have stopped"
      handleNext (Left stopped) = do
        warnStopped stopped
        warnHalting
      handleNext (Right ((total, stopped), updated)) = do
        clearScreen
        warnStopped stopped
        showTotal total
        go updated
      go initial = unfold initial >>= handleNext
  go bud


warnStopped :: [ProcessID] -> IO ()
warnStopped pids = unless (null pids) $ do
  let errMsg = "some processes stopped:pids:" +| toInteger <$> pids |+ ""
  errStrLn False errMsg


-- | The name of a process or program in the memory report.
type ProcName = Text


-- | Like @'unfoldMemUsageAfter'@, but uses the default choices for naming and indindexing the reported programs
unfoldMemUsageAfter ::
  (Integral seconds) =>
  seconds ->
  ReportBud ->
  IO (Either [ProcessID] ((Map Text MemUsage, [ProcessID]), ReportBud))
unfoldMemUsageAfter = unfoldMemUsageAfter' nameFor dropId


-- | Like @'unfoldMemUsage'@ but computes the @'MemUsage's@ after a delay
unfoldMemUsageAfter' ::
  (Ord a, Integral seconds) =>
  (ProcessID -> IO (Either LostPid ProcName)) ->
  ((ProcessID, ProcName, PerProc) -> (a, PerProc)) ->
  seconds ->
  ReportBud ->
  IO (Either [ProcessID] ((Map a MemUsage, [ProcessID]), ReportBud))
unfoldMemUsageAfter' namer mkCmd spanSecs bud = do
  let spanMicros = 1000000 * fromInteger (toInteger spanSecs)
  threadDelay spanMicros
  unfoldMemUsage namer mkCmd bud


{- | Unfold @'MemUsage's@ specified by a @'ReportBud'@

The @ProcessID@ of stopped processes are reported, both as part of intermediate
invocations (via the @[ProcessID]@ in the @Right@), and in the final one (as the
value of the @Left@).
-}
unfoldMemUsage ::
  (Ord a) =>
  (ProcessID -> IO (Either LostPid ProcName)) ->
  ((ProcessID, ProcName, PerProc) -> (a, PerProc)) ->
  ReportBud ->
  IO (Either [ProcessID] ((Map a MemUsage, [ProcessID]), ReportBud))
unfoldMemUsage namer mkCmd bud = do
  let changePids rbPids = bud {rbPids}
      dropStopped t [] = Just t
      dropStopped ReportBud {rbPids = ps} stopped =
        changePids <$> nonEmpty (NE.filter (`notElem` stopped) ps)
      ReportBud {rbPids = pids, rbHasPss = hasPss} = bud
      nextState (stopped, []) = Left stopped
      nextState (stopped, xs) = case dropStopped bud stopped of
        Just updated -> Right ((amass hasPss (map mkCmd xs), stopped), updated)
        Nothing -> Left stopped
  nextState <$> foldlEitherM' (readNameAndStats namer bud) pids


-- | Load the @'MemUsage'@ of a program specified by its @ProcessID@
readForOnePid :: ProcessID -> IO (Either NotRun (ProcName, MemUsage))
readForOnePid pid = do
  let mkBud' xs = mkReportBud xs <&> maybe (Left OddKernel) Right
      noProc = NoProc pid
      fromMemUsage x = maybe (Left $ PidLost noProc) Right (Map.lookupMin x)
      andFromUsage = either (Left . PidLost) fromMemUsage
  nameFor pid >>= \case
    Left err -> pure $ Left $ PidLost err
    Right _ ->
      mkBud' (NE.singleton pid) >>= \case
        Left err -> pure $ Left err
        Right bud -> readMemUsage bud <&> andFromUsage


{- | Like @'readMemUsage'@ but uses the default choices for indexing
programs/processes
-}
readMemUsage :: ReportBud -> IO (Either LostPid (Map ProcName MemUsage))
readMemUsage = readMemUsage' nameFor dropId


{- | Loads the @'MemUsage'@ specified by a @'ReportBud'@

Fails if

- the system does not have the expected /proc filesystem with memory records
- any of the processes in @'ReportBud'@ are missing or inaccessible
-}
readMemUsage' ::
  Ord a =>
  (ProcessID -> IO (Either LostPid ProcName)) ->
  ((ProcessID, ProcName, PerProc) -> (a, PerProc)) ->
  ReportBud ->
  IO (Either LostPid (Map a MemUsage))
readMemUsage' namer mkCmd bud = do
  let amass' cmds = amass (rbHasPss bud) $ map mkCmd cmds
  fmap amass' <$> foldlEitherM (readNameAndStats namer bud) (rbPids bud)


readNameAndStats ::
  (ProcessID -> IO (Either LostPid ProcName)) ->
  ReportBud ->
  ProcessID ->
  IO (Either LostPid (ProcessID, ProcName, PerProc))
readNameAndStats namer bud pid = do
  namer pid >>= \case
    Left e -> pure $ Left e
    Right name ->
      readMemStats bud pid >>= \case
        Left e -> pure $ Left e
        Right stats -> pure $ Right (pid, name, stats)


reportFlaws :: ReportBud -> Bool -> Bool -> IO ()
reportFlaws bud showSwap onlyTotal = do
  let reportSwap = errStrLn onlyTotal . fmtSwapFlaws
      reportRam = errStrLn onlyTotal . fmtRamFlaws
      (ram, swap) = (rbRamFlaws bud, rbSwapFlaws bud)
  -- when showSwap, report swap flaws
  -- unless (showSwap and onlyTotal), show ram flaws
  when showSwap $ maybe (pure ()) reportSwap swap
  unless (onlyTotal && showSwap) $ maybe (pure ()) reportRam ram


verify :: Choices -> IO ReportBud
verify cs = verify' (choicePidsToShow cs) >>= either (haltErr . fmtNotRun) pure


verify' :: Maybe (NonEmpty ProcessID) -> IO (Either NotRun ReportBud)
verify' pidsMb = do
  let mkBud' xs = mkReportBud xs <&> maybe (Left OddKernel) Right
      thenMkBud = either (pure . Left) mkBud'
  case pidsMb of
    Just pids -> checkAllExist pids >>= thenMkBud
    Nothing -> whenRoot $ allKnownProcs >>= thenMkBud


procRoot :: String
procRoot = "/proc/"


pidPath :: String -> ProcessID -> FilePath
pidPath base pid = "" +| procRoot |++| toInteger pid |+ "/" +| base |+ ""


whenRoot :: IO (Either NotRun a) -> IO (Either NotRun a)
whenRoot action = do
  -- if choicePidsToShow is Nothing, must be running as root
  isRoot' <- (== 0) <$> getEffectiveUserID
  if isRoot' then action else pure $ Left NeedsRoot


{- |  pidExists returns false for any ProcessID that does not exist or cannot
be accessed
-}
pidExeExists :: ProcessID -> IO Bool
pidExeExists = fmap (either (const False) (const True)) . exeInfo


-- | Obtain the @ProcName@ as the full cmd path
nameAsFullCmd :: ProcessID -> IO (Either LostPid ProcName)
nameAsFullCmd pid = do
  let cmdlinePath = pidPath "cmdline" pid
      err = NoCmdLine pid
      recombine = Text.intercalate " " . NE.toList
      orLostPid = maybe (Left err) (Right . recombine)
  readUtf8Text cmdlinePath >>= (pure . orLostPid) . parseCmdline


-- | Obtain the @ProcName@ by examining the path linked by @{proc_root}/pid/exe@
nameFromExeOnly :: ProcessID -> IO (Either LostPid ProcName)
nameFromExeOnly pid = do
  exeInfo pid >>= \case
    Right i | not $ eiDeleted i -> pure $ Right $ baseName $ eiOriginal i
    -- when the exe bud ends with (deleted), the version of the exe used to
    -- invoke the process has been removed from the filesystem. Sometimes it has
    -- been updated; examining both the original bud and the version in
    -- cmdline help determine what occurred
    Right ExeInfo {eiOriginal = orig} ->
      exists orig >>= \case
        True -> pure $ Right $ baseName $ "" +| orig |+ " [updated]"
        _ -> do
          let cmdlinePath = pidPath "cmdline" pid
          readUtf8Text cmdlinePath <&> parseCmdline >>= \case
            Just (x :| _) -> do
              let addSuffix' b = x <> if b then " [updated]" else " [deleted]"
              Right . baseName . addSuffix' <$> exists x
            -- args should not be empty when {pid_root}/exe resolves to a
            -- path, it's an error if it is
            Nothing -> pure $ Left $ NoCmdLine pid
    Left e -> pure $ Left e


{- | Obtain the @ProcName@ by examining the path linked by @{proc_root}/pid/exe@
or its parent's name if that is a better match
-}
nameFor :: ProcessID -> IO (Either LostPid ProcName)
nameFor pid =
  nameFromExeOnly pid
    >>= either (pure . Left) (parentNameIfMatched pid)


parentNameIfMatched :: ProcessID -> Text -> IO (Either LostPid ProcName)
parentNameIfMatched pid candidate = do
  let isMatch = flip Text.isPrefixOf candidate . siName
  statusInfo pid >>= \case
    Left err -> pure $ Left err
    Right si | isMatch si -> pure $ Right candidate
    Right si ->
      nameFromExeOnly (siParent si) >>= \case
        Right n | n == candidate -> pure $ Right n
        _ -> pure $ Right $ siName si


-- | Represents errors that prevent a report from being generated
data NotRun
  = PidLost LostPid
  | MissingPids (NonEmpty ProcessID)
  | NeedsRoot
  | OddKernel
  | NoRecords
  deriving (Eq, Show)


fmtNotRun :: NotRun -> Text
fmtNotRun NeedsRoot = "run as root when no pids are specified using -p"
fmtNotRun (PidLost x) = fmtLostPid x
fmtNotRun OddKernel = "unrecognized kernel version"
fmtNotRun (MissingPids pids) = "no records available for: " +| listF (toInteger <$> pids) |+ ""
fmtNotRun NoRecords = "could not find any process records"


{- | Represents reasons a specified @pid =`ProcessID`@ may be not have memory
records.
-}
data LostPid
  = NoExeFile ProcessID
  | NoStatusCmd ProcessID
  | NoStatusParent ProcessID
  | NoCmdLine ProcessID
  | BadStatm ProcessID
  | NoProc ProcessID
  deriving (Eq, Show)


fmtLostPid :: LostPid -> Text
fmtLostPid (NoStatusCmd pid) = "missing:no name in {proc_root}/" +| toInteger pid |+ "/status"
fmtLostPid (NoStatusParent pid) = "missing:no ppid in {proc_root}/" +| toInteger pid |+ "/status"
fmtLostPid (NoExeFile pid) = "missing:{proc_root}/" +| toInteger pid |+ "/exe"
fmtLostPid (NoCmdLine pid) = "missing:{proc_root}/" +| toInteger pid |+ "/cmdline"
fmtLostPid (NoProc pid) = "missing:memory records for pid:" +| toInteger pid |+ ""
fmtLostPid (BadStatm pid) = "missing:invalid memory record in {proc_root}/" +| toInteger pid |+ "/statm"


haltLostPid :: LostPid -> IO a
haltLostPid err = do
  Text.hPutStrLn stderr $ "halting due to " +| fmtLostPid err |+ ""
  exitFailure


exeInfo :: ProcessID -> IO (Either LostPid ExeInfo)
exeInfo pid = do
  let exePath = pidPath "exe" pid
      handledErr e = isDoesNotExistError e || isPermissionError e
      onIOE e = if handledErr e then pure (Left $ NoExeFile pid) else throwIO e
  handle onIOE $ do
    Right . parseExeInfo . Text.pack <$> getSymbolicLinkTarget exePath


exists :: Text -> IO Bool
exists = doesFileExist . Text.unpack


statusInfo :: ProcessID -> IO (Either LostPid StatusInfo)
statusInfo pid = do
  let statusPath = pidPath "status" pid
      fromBadStatus NoCmd = NoStatusCmd pid
      fromBadStatus NoParent = NoStatusParent pid
  first fromBadStatus . parseStatusInfo <$> readUtf8Text statusPath


parseCmdline :: Text -> Maybe (NonEmpty Text)
parseCmdline =
  let split' = Text.split isNullOrSpace . Text.strip . Text.dropWhileEnd isNull
   in nonEmpty . split'


nonExisting :: NonEmpty ProcessID -> IO [ProcessID]
nonExisting = filterM (fmap not . pidExeExists) . NE.toList


checkAllExist :: NonEmpty ProcessID -> IO (Either NotRun (NonEmpty ProcessID))
checkAllExist pids =
  nonExisting pids >>= \case
    [] -> pure $ Right pids
    x : xs -> pure $ Left $ MissingPids $ x :| xs


allKnownProcs :: IO (Either NotRun (NonEmpty ProcessID))
allKnownProcs =
  let readNaturals = fmap (mapMaybe readMaybe)
      orNoPids = maybe (Left NoRecords) Right
   in readNaturals (listDirectory procRoot)
        >>= filterM pidExeExists
        >>= pure . orNoPids . nonEmpty


baseName :: Text -> Text
baseName = Text.pack . takeBaseName . Text.unpack


readMemStats :: ReportBud -> ProcessID -> IO (Either LostPid PerProc)
readMemStats bud pid = do
  statmExists <- doesFileExist $ pidPath "statm" pid
  if
      | rbHasSmaps bud -> Right . parseFromSmap <$> readSmaps pid
      | statmExists -> do
          let readStatm' = readUtf8Text $ pidPath "statm" pid
              orLostPid = maybe (Left $ BadStatm pid) Right
          orLostPid . parseFromStatm (rbKernel bud) <$> readStatm'
      | otherwise -> pure $ Left $ NoProc pid


readSmaps :: ProcessID -> IO Text
readSmaps pid = do
  let smapPath = pidPath "smaps" pid
      rollupPath = pidPath "smaps_rollup" pid
  hasSmaps <- doesFileExist smapPath
  hasRollup <- doesFileExist rollupPath
  if
      | hasRollup -> readUtf8Text rollupPath
      | hasSmaps -> readUtf8Text smapPath
      | otherwise -> pure Text.empty


overallTotals :: [MemUsage] -> (Int, Int)
overallTotals cts =
  let step (private, swap) ct = (private + muPrivate ct, swap + muSwap ct)
   in foldl' step (0, 0) cts


fmtMemBytes :: Int -> Text
fmtMemBytes x = "" +| x * 1024 |+ ""


foldlEitherM ::
  (Foldable t, Monad m) =>
  (a -> m (Either b c)) ->
  t a ->
  m (Either b [c])
foldlEitherM f xs =
  let go (Left err) _ = pure $ Left err
      go (Right acc) a =
        f a >>= \case
          Left err -> pure $ Left err
          Right y -> pure $ Right (y : acc)
   in foldlM go (Right []) xs


foldlEitherM' ::
  (Foldable t, Monad m) =>
  (a -> m (Either b c)) ->
  t a ->
  m ([a], [c])
foldlEitherM' f xs =
  let
    go (as, cs) a =
      f a >>= \case
        Left _ -> pure (a : as, cs)
        Right c -> pure (as, c : cs)
   in
    foldlM go (mempty, mempty) xs


haltErr :: Text -> IO a
haltErr err = do
  errStrLn True err
  exitFailure


errStrLn :: Bool -> Text -> IO ()
errStrLn errOrWarn txt = do
  let prefix = if errOrWarn then "error: " else "warning: "
  Text.hPutStrLn stderr $ prefix <> txt


{- | Index a @'PerProc'@ using the program name and process ID.

All @PerProc's@ are distinct with the when added to the @MemUsage@
-}
withPid :: (ProcessID, ProcName, PerProc) -> ((ProcessID, ProcName), PerProc)
withPid (pid, name, pp) = ((pid, name), pp)


{- | Index a @'PerProc'@ using just the program name

@PerProc's@ with the same @ProcName@ will be merged when added to a @MemUsage@
-}
dropId :: (ProcessID, ProcName, PerProc) -> (ProcName, PerProc)
dropId (_pid, name, pp) = (name, pp)
