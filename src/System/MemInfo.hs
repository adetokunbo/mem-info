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

Implements __printmem__, a command that computes the memory usage of some
processes
-}
module System.MemInfo (
  -- * Implement __printmem__
  getChoices,
  printProcs,

  -- * Read /MemUsage/
  readForOnePid,
  readMemUsage',
  readMemUsage,
  NotRun (..),
  LostPid (..),

  -- * Stream /MemUsage/ periodically
  unfoldMemUsage,
  unfoldMemUsageAfter',
  unfoldMemUsageAfter,

  -- * Obtain the process/program name
  ProcNamer,
  nameFromExeOnly,
  nameFor,
  nameAsFullCmd,

  -- * Index by pid or name
  ProcName,
  Indexer,
  dropId,
  withPid,

  -- * Print /MemUsage/
  printUsage',
  printUsage,

  -- * Convenient re-exports
  mkReportBud,
  ProcessID,
  AsCmdName (..),
) where

import Data.Bifunctor (Bifunctor (..))
import Data.Functor ((<&>))
import Data.List (sortBy)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Ord (Down (..), comparing)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Fmt (
  listF,
  (+|),
  (|+),
  (|++|),
 )
import System.Exit (exitFailure)
import System.MemInfo.Choices (
  Choices (..),
  PrintOrder (..),
  Style (..),
  getChoices,
 )
import System.MemInfo.Prelude
import System.MemInfo.Print (
  AsCmdName (..),
  fmtMemUsage,
  styleOutput,
 )
import System.MemInfo.Proc (
  BadStatus (..),
  ExeInfo (..),
  MemUsage (..),
  ProcUsage (..),
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


{- | Print a report to @stdout@ displaying the memory usage of the programs
specified by @Choices@
-}
printProcs :: Choices -> IO ()
printProcs cs@Choices {choiceByPid = byPid} = do
  bud <- verify cs
  if byPid
    then printProcs' withPid bud cs
    else printProcs' dropId bud cs


printProcs' :: (Ord a, AsCmdName a) => Indexer a -> ReportBud -> Choices -> IO ()
printProcs' indexer bud cs = do
  let Choices
        { choiceShowSwap = showSwap
        , choiceOnlyTotal = onlyTotal
        , choiceWatchSecs = watchSecsMb
        , choicePrintOrder = printOrder
        , choiceReversed = reversed
        , choiceStyle = style
        } = cs
      style' = fromMaybe Normal style
      toList = sortBy (byPrintOrder' reversed printOrder) . Map.toList
      printEachCmd = printMemUsages bud style' showSwap onlyTotal . toList
      printTheTotal = onlyPrintTotal bud showSwap onlyTotal . toList
      showTotal = if onlyTotal then printTheTotal else printEachCmd
      namer = if choiceSplitArgs cs then nameAsFullCmd else nameFor
  case watchSecsMb of
    Nothing -> readMemUsage' namer indexer bud >>= either haltLostPid showTotal
    (Just spanSecs) -> do
      let unfold = unfoldMemUsageAfter' namer indexer spanSecs
      loopPrintMemUsages unfold bud showTotal


printMemUsages ::
  (AsCmdName a) =>
  ReportBud ->
  Style ->
  Bool ->
  Bool ->
  [(a, MemUsage)] ->
  IO ()
printMemUsages bud style showSwap onlyTotal totals = do
  let overallIsAccurate = (showSwap && rbHasSwapPss bud) || rbHasPss bud
      output = styleOutput showSwap style overallIsAccurate totals
  mapM_ Text.putStrLn output
  reportFlaws bud showSwap onlyTotal


-- | Print the program name and memory usage, optionally hiding the swap size
printUsage' :: (AsCmdName a) => (a, MemUsage) -> Bool -> IO ()
printUsage' (name, mu) showSwap = Text.putStrLn $ fmtMemUsage showSwap name mu


-- | Like @'printUsage''@, but alway shows the swap size
printUsage :: (AsCmdName a) => (a, MemUsage) -> IO ()
printUsage = flip printUsage' True


onlyPrintTotal :: (AsCmdName k) => ReportBud -> Bool -> Bool -> [(k, MemUsage)] -> IO ()
onlyPrintTotal bud showSwap onlyTotal totals = do
  let (private, swap) = overallTotals $ map snd totals
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


-- | Like @'unfoldMemUsageAfter''@, but uses the default 'ProcNamer' and 'Indexer'
unfoldMemUsageAfter ::
  (Integral seconds) =>
  seconds ->
  ReportBud ->
  IO (Either [ProcessID] ((Map ProcName MemUsage, [ProcessID]), ReportBud))
unfoldMemUsageAfter = unfoldMemUsageAfter' nameFor dropId


-- | Like @'unfoldMemUsage'@ but computes the @'MemUsage's@ after a delay
unfoldMemUsageAfter' ::
  (Ord a, AsCmdName a, Integral seconds) =>
  ProcNamer ->
  Indexer a ->
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
  ProcNamer ->
  Indexer a ->
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
      mkBud' (pid :| []) >>= \case
        Left err -> pure $ Left err
        Right bud -> readMemUsage bud <&> andFromUsage


-- | Like @'readMemUsage''@ but uses the default 'ProcNamer' and 'Indexer'
readMemUsage :: ReportBud -> IO (Either LostPid (Map ProcName MemUsage))
readMemUsage = readMemUsage' nameFor dropId


{- | Loads the @'MemUsage'@ specified by a @'ReportBud'@

Fails if

- the system does not have the expected /proc filesystem memory records
- any of the processes specified by @'ReportBud'@ are missing or inaccessible
-}
readMemUsage' ::
  (Ord a) =>
  ProcNamer ->
  Indexer a ->
  ReportBud ->
  IO (Either LostPid (Map a MemUsage))
readMemUsage' namer mkCmd bud = do
  let amass' cmds = amass (rbHasPss bud) $ map mkCmd cmds
  fmap amass' <$> foldlEitherM (readNameAndStats namer bud) (rbPids bud)


readNameAndStats ::
  ProcNamer ->
  ReportBud ->
  ProcessID ->
  IO (Either LostPid (ProcessID, ProcName, ProcUsage))
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
    Nothing -> allKnownProcs >>= thenMkBud


procRoot :: String
procRoot = "/proc/"


pidPath :: String -> ProcessID -> FilePath
pidPath base pid = "" +| procRoot |++| toInteger pid |+ "/" +| base |+ ""


{- | pidExists returns false for any ProcessID that does not exist or cannot
be accessed
-}
pidExeExists :: ProcessID -> IO Bool
pidExeExists = fmap (either (const False) (const True)) . exeInfo


-- | Obtain the @ProcName@ as the full cmd path
nameAsFullCmd :: ProcNamer
nameAsFullCmd pid = do
  let
    err = NoCmdLine pid
    recombine = Text.intercalate " " . NE.toList
    orLostPid = maybe (Left err) (Right . recombine)
  readCmdlinePath pid >>= (pure . orLostPid) . parseCmdline


readCmdlinePath :: ProcessID -> IO Text
readCmdlinePath pid = readUtf8Text $ pidPath "cmdline" pid


{- | Obtain the @ProcName@ by examining the path linked by
__{proc_root}\/pid\/exe__
-}
nameFromExeOnly :: ProcNamer
nameFromExeOnly pid = do
  let pickSuffix = \case
        Just (x :| _) -> do
          let addSuffix' b = x <> if b then " [updated]" else " [deleted]"
          Right . baseName . addSuffix' <$> exists x
        -- args should not be empty when {pid_root}/exe resolves to a
        -- path, it's an error if it is
        Nothing -> pure $ Left $ NoCmdLine pid

  exeInfo pid >>= \case
    Left e -> pure $ Left e
    Right i | not $ eiDeleted i -> pure $ Right $ baseName $ eiOriginal i
    -- when the exe bud ends with (deleted), the version of the exe used to
    -- invoke the process has been removed from the filesystem. Sometimes it has
    -- been updated; examining both the original bud and the version in
    -- cmdline help determine what occurred
    Right ExeInfo {eiOriginal = orig} ->
      exists orig >>= \wasUpdated ->
        if wasUpdated
          then pure $ Right $ baseName $ "" +| orig |+ " [updated]"
          else readCmdlinePath pid >>= pickSuffix . parseCmdline


-- | Functions that obtain a process name given its @pid@
type ProcNamer = ProcessID -> IO (Either LostPid ProcName)


{- | Obtain the @ProcName@ by examining the path linked by
__{proc_root}\/pid\/exe__ or its parent's name if that is a better match
-}
nameFor :: ProcNamer
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
        _anyLostPid -> pure $ Right $ siName si


-- | Represents errors that prevent a report from being generated
data NotRun
  = PidLost !LostPid
  | MissingPids !(NonEmpty ProcessID)
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


{- | Represents reasons a specified @pid@ may not have memory
records.
-}
data LostPid
  = NoExeFile !ProcessID
  | NoStatusCmd !ProcessID
  | NoStatusParent !ProcessID
  | NoCmdLine !ProcessID
  | BadStatm !ProcessID
  | NoProc !ProcessID
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
        >>= pure
        . orNoPids
        . nonEmpty


baseName :: Text -> Text
baseName = Text.pack . takeBaseName . Text.unpack


readMemStats :: ReportBud -> ProcessID -> IO (Either LostPid ProcUsage)
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


-- | Functions that generate the report index
type Indexer index = (ProcessID, ProcName, ProcUsage) -> (index, ProcUsage)


{- | Index a @'ProcUsage'@ using the program name and process ID.

Each @ProcUsage@ remains distinct when added to a @MemUsage@
-}
withPid :: Indexer (ProcessID, ProcName)
withPid (pid, name, pp) = ((pid, name), pp)


{- | Index a @'ProcUsage'@ using just the program name

@ProcUsage's@ with the same @ProcName@ will be merged when added to a @MemUsage@
-}
dropId :: Indexer ProcName
dropId (_pid, name, pp) = (name, pp)


byPrintOrder ::
  (Ord c) =>
  (((c, MemUsage) -> Int) -> (c, MemUsage) -> (c, MemUsage) -> Ordering) ->
  PrintOrder ->
  (c, MemUsage) ->
  (c, MemUsage) ->
  Ordering
byPrintOrder f Swap = f $ muSwap . snd
byPrintOrder f Shared = f $ muShared . snd
byPrintOrder f Private = f $ muPrivate . snd
byPrintOrder f Count = f $ muCount . snd


byPrintOrder' ::
  (Ord a) =>
  Bool ->
  Maybe PrintOrder ->
  (a, MemUsage) ->
  (a, MemUsage) ->
  Ordering
byPrintOrder' reversed mbOrder =
  let cmpUsage = if reversed then comparing else comparing'
      cmpName = if reversed then comparing else comparing'
      byName = cmpName fst
      byUsage = byPrintOrder cmpUsage
   in maybe byName byUsage mbOrder


comparing' :: (Ord a) => (b -> a) -> b -> b -> Ordering
comparing' f a b = compare (Down $ f a) (Down $ f b)
