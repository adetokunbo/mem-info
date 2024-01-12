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
  getChoices,
  printProcs,
) where

import Data.Bifunctor (Bifunctor (..), first)
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
  fmtCmdTotal,
  fmtOverall,
 )
import System.MemInfo.Proc (
  BadStatus (..),
  CmdTotal (..),
  ExeInfo (..),
  PerProc (..),
  StatusInfo (..),
  amass,
  parseExeInfo,
  parseFromSmap,
  parseFromStatm,
  parseStatusInfo,
 )
import System.MemInfo.SysInfo (KernelVersion, fickleSharing, readKernelVersion)
import System.Posix.User (getEffectiveUserID)


-- | Report on the memory usage of the processes specified by @Choices@
printProcs :: Choices -> IO ()
printProcs cs = do
  target <- verify cs
  let showSwap = choiceShowSwap cs
      onlyTotal = choiceOnlyTotal cs
      withPid (pid, name, stats) = ((pid, name), stats)
      dropId (_, name, stats) = (name, stats)
      printEachCmd totals = printCmdTotals target showSwap onlyTotal totals
      printTheTotal = onlyPrintTotal target showSwap onlyTotal
      showTotal cmds = if onlyTotal then printTheTotal cmds else printEachCmd cmds
      namer = if choiceSplitArgs cs then nameAsFullCmd else nameFor
  if choiceByPid cs
    then case choiceWatchSecs cs of
      Nothing -> readCmdTotal target namer withPid >>= either haltLostPid showTotal
      Just period -> do
        let unfold = unfoldCmdTotalAfter period namer withPid
        loopShowingTotals unfold target showTotal
    else case choiceWatchSecs cs of
      Nothing -> readCmdTotal target namer dropId >>= either haltLostPid showTotal
      Just period -> do
        let unfold = unfoldCmdTotalAfter period namer dropId
        loopShowingTotals unfold target showTotal


printCmdTotals :: AsCmdName a => Target -> Bool -> Bool -> Map a CmdTotal -> IO ()
printCmdTotals target showSwap onlyTotal totals = do
  let overall = overallTotals $ Map.elems totals
      overallIsAccurate = (showSwap && tHasSwapPss target) || tHasPss target
      print' (name, stats) = Text.putStrLn $ fmtCmdTotal showSwap name stats
  Text.putStrLn $ fmtAsHeader showSwap
  mapM_ print' $ Map.toList totals
  when overallIsAccurate $ Text.putStrLn $ fmtOverall showSwap overall
  reportFlaws target showSwap onlyTotal


onlyPrintTotal :: Target -> Bool -> Bool -> Map k CmdTotal -> IO ()
onlyPrintTotal target showSwap onlyTotal totals = do
  let (private, swap) = overallTotals $ Map.elems totals
      printRawTotal = Text.putStrLn . fmtMemBytes
  if showSwap
    then do
      when (tHasSwapPss target) $ printRawTotal swap
      reportFlaws target showSwap onlyTotal
      when (isJust $ tSwapFlaw target) exitFailure
    else do
      when (tHasPss target) $ printRawTotal private
      reportFlaws target showSwap onlyTotal
      when (isJust $ tRamFlaw target) exitFailure


loopShowingTotals ::
  (Ord c, AsCmdName c) =>
  (Target -> IO ([ProcessID], Maybe (Map c CmdTotal, Target))) ->
  Target ->
  (Map c CmdTotal -> IO ()) ->
  IO ()
loopShowingTotals unfold target showTotal = do
  let clearScreen = putStrLn "\o033c"
      errHalting = errStrLn False "halting: all monitored processes have stopped"
      handleNext (stopped, nextMb) = do
        warnStopped stopped
        flip (maybe errHalting) nextMb $ \(total, updated) -> do
          clearScreen
          showTotal total
          go updated
      go initial = unfold initial >>= handleNext
  go target


warnStopped :: [ProcessID] -> IO ()
warnStopped pids = unless (null pids) $ do
  let errMsg = "some processes stopped and will no longer appear:pids:" +| toInteger <$> pids |+ ""
  errStrLn False errMsg


unfoldCmdTotalAfter ::
  (Ord a, Integral p) =>
  p ->
  (ProcessID -> IO (Either LostPid Text)) ->
  ((ProcessID, Text, PerProc) -> (a, PerProc)) ->
  Target ->
  IO ([ProcessID], Maybe (Map a CmdTotal, Target))
unfoldCmdTotalAfter spanSecs namer mkCmd target = do
  let spanMicros = 1000000 * fromInteger (toInteger spanSecs)
      changePids tPids = target {tPids}
      dropStopped t [] = Just t
      dropStopped Target {tPids = ps} stopped =
        changePids <$> nonEmpty (NE.filter (`notElem` stopped) ps)
      Target {tPids = pids, tHasPss = hasPss} = target
      resOf _ [] = Nothing
      resOf stopped xs = case dropStopped target stopped of
        Just updated -> Just (amass hasPss (map mkCmd xs), updated)
        Nothing -> Nothing
      nextState (stopped, xs) = (stopped, resOf stopped xs)

  threadDelay spanMicros
  nextState <$> foldlEitherM' (readNameAndStats namer target) pids


readCmdTotal ::
  Ord a =>
  Target ->
  (ProcessID -> IO (Either LostPid Text)) ->
  ((ProcessID, Text, PerProc) -> (a, PerProc)) ->
  IO (Either LostPid (Map a CmdTotal))
readCmdTotal target namer mkCmd = do
  let amass' cmds = amass (tHasPss target) $ map mkCmd cmds
  fmap amass' <$> foldlEitherM (readNameAndStats namer target) (tPids target)


readNameAndStats ::
  (ProcessID -> IO (Either LostPid Text)) ->
  Target ->
  ProcessID ->
  IO (Either LostPid (ProcessID, Text, PerProc))
readNameAndStats namer target pid = do
  namer pid >>= \case
    Left e -> pure $ Left e
    Right name ->
      readMemStats target pid >>= \case
        Left e -> pure $ Left e
        Right stats -> pure $ Right (pid, name, stats)


reportFlaws :: Target -> Bool -> Bool -> IO ()
reportFlaws target showSwap onlyTotal = do
  let reportSwap = errStrLn onlyTotal . fmtSwapFlaw
      reportRam = errStrLn onlyTotal . fmtRamFlaw
      (ram, swap) = (tRamFlaw target, tSwapFlaw target)
  -- when showSwap, report swap flaws
  -- unless (showSwap and onlyTotal), show ram flaws
  when showSwap $ maybe (pure ()) reportSwap swap
  unless (onlyTotal && showSwap) $ maybe (pure ()) reportRam ram


-- | Represents the information needed to generate a memory usage report
data Target = Target
  { tPids :: !(NonEmpty ProcessID)
  , tKernel :: !KernelVersion
  , tHasPss :: !Bool
  , tHasSwapPss :: !Bool
  , tHasSmaps :: !Bool
  , tRamFlaw :: Maybe RamFlaw
  , tSwapFlaw :: Maybe SwapFlaw
  }
  deriving (Eq, Show)


verify :: Choices -> IO Target
verify cs = case choicePidsToShow cs of
  Just tPids -> do
    -- halt if any specified pid cannot be accessed
    checkAllExist tPids
    mkTarget tPids
  Nothing -> do
    -- if choicePidsToShow is Nothing, must be running as root
    isRoot' <- isRoot
    unless isRoot' $ haltErr "run as root when no pids are specified using -p"
    allKnownProcs >>= mkTarget


mkTarget :: NonEmpty ProcessID -> IO Target
mkTarget tPids = do
  let firstPid = NE.head tPids
      smapsPath = pidPath "smaps" firstPid
      hasPss = Text.isInfixOf "Pss:"
      hasSwapPss = Text.isInfixOf "SwapPss:"
      memtypes x = (hasPss x, hasSwapPss x)
  tKernel <- readKernelVersion >>= either haltErr pure
  tHasSmaps <- doesFileExist smapsPath
  (tHasPss, tHasSwapPss) <- memtypes <$> readUtf8Text smapsPath
  checkForFlaws $
    Target
      { tPids
      , tKernel
      , tHasPss
      , tHasSwapPss
      , tHasSmaps
      , tRamFlaw = Nothing
      , tSwapFlaw = Nothing
      }


procRoot :: String
procRoot = "/proc/"


pidPath :: String -> ProcessID -> FilePath
pidPath base pid = "" +| procRoot |++| toInteger pid |+ "/" +| base |+ ""


isRoot :: IO Bool
isRoot = (== 0) <$> getEffectiveUserID


{- |  pidExists returns false for any ProcessID that does not exist or cannot
be accessed
-}
pidExeExists :: ProcessID -> IO Bool
pidExeExists = fmap (either (const False) (const True)) . exeInfo


nameAsFullCmd :: ProcessID -> IO (Either LostPid Text)
nameAsFullCmd pid = do
  let cmdlinePath = pidPath "cmdline" pid
      err = NoCmdLine pid
      recombine = Text.intercalate " " . NE.toList
      orLostPid = maybe (Left err) (Right . recombine)
  readUtf8Text cmdlinePath >>= (pure . orLostPid) . parseCmdline


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


nameFor :: ProcessID -> IO (Either LostPid Text)
nameFor pid =
  nameFromExeOnly pid
    >>= either (pure . Left) (parentNameIfMatched pid)


parentNameIfMatched :: ProcessID -> Text -> IO (Either LostPid Text)
parentNameIfMatched pid candidate = do
  let isMatch = flip Text.isPrefixOf candidate . siName
  statusInfo pid >>= \case
    Left err -> pure $ Left err
    Right si | isMatch si -> pure $ Right candidate
    Right si ->
      nameFromExeOnly (siParent si) >>= \case
        Right n | n == candidate -> pure $ Right n
        _ -> pure $ Right $ siName si


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


checkAllExist :: NonEmpty ProcessID -> IO ()
checkAllExist pids =
  nonExisting pids >>= \case
    [] -> pure ()
    xs -> haltErr $ "no records available for: " +| listF (toInteger <$> xs) |+ ""


allKnownProcs :: IO (NonEmpty ProcessID)
allKnownProcs =
  let readNaturals = fmap (mapMaybe readMaybe)
      orNoPids = flip maybe pure $ haltErr "could not find any process records"
   in readNaturals (listDirectory procRoot)
        >>= filterM pidExeExists
        >>= orNoPids . nonEmpty


baseName :: Text -> Text
baseName = Text.pack . takeBaseName . Text.unpack


readMemStats :: Target -> ProcessID -> IO (Either LostPid PerProc)
readMemStats target pid = do
  statmExists <- doesFileExist $ pidPath "statm" pid
  if
      | tHasSmaps target -> Right . parseFromSmap <$> readSmaps pid
      | statmExists -> do
          let readStatm' = readUtf8Text $ pidPath "statm" pid
              orLostPid = maybe (Left $ BadStatm pid) Right
          orLostPid . parseFromStatm (tKernel target) <$> readStatm'
      | otherwise -> pure $ Left $ NoProc pid


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


checkForFlaws :: Target -> IO Target
checkForFlaws target = do
  let pid = NE.head $ tPids target
      version = tKernel target
      fickleShared = fickleSharing version
      Target
        { tHasPss = hasPss
        , tHasSmaps = hasSmaps
        , tHasSwapPss = hasSwapPss
        } = target
  (tRamFlaw, tSwapFlaw) <- case version of
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
      let withSmaps = if hasPss then best else alt
          alt = (Just ExactForIsolatedMem, Just ExactForIsolatedSwap)
          best = (Nothing, Just ExactForIsolatedSwap)
          withNoSmaps = Just $ if fickleShared then NoSharedMem else SomeSharedMem
      pure $ if hasSmaps then withSmaps else (withNoSmaps, Just NoSwap)
    (major, _, _) | major > 2 && hasSmaps -> do
      let alt = (Nothing, Just ExactForIsolatedSwap)
          best = (Nothing, Nothing)
      pure $ if hasSwapPss then best else alt
    _ -> pure (Just ExactForIsolatedMem, Just NoSwap)
  pure $ target {tRamFlaw, tSwapFlaw}


overallTotals :: [CmdTotal] -> (Int, Int)
overallTotals cts =
  let step (private, swap) ct = (private + ctPrivate ct, swap + ctSwap ct)
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
