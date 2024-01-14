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
  readCmdTotal,
  readPidTotal,
  unfoldCmdTotalAfter,
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
import System.MemInfo.SysInfo (
  Target (..),
  fmtRamFlaw,
  fmtSwapFlaw,
  mkTarget,
 )
import System.Posix.User (getEffectiveUserID)


-- | The derived name of a process or program in the memory report.
type ProcName = Text


-- | Report on the memory usage of the processes specified by @Choices@
printProcs :: Choices -> IO ()
printProcs cs = do
  target <- verify cs
  let showSwap = choiceShowSwap cs
      onlyTotal = choiceOnlyTotal cs
      printEachCmd totals = printCmdTotals target showSwap onlyTotal totals
      printTheTotal = onlyPrintTotal target showSwap onlyTotal
      showTotal cmds = if onlyTotal then printTheTotal cmds else printEachCmd cmds
      namer = if choiceSplitArgs cs then nameAsFullCmd else nameFor
  if choiceByPid cs
    then case choiceWatchSecs cs of
      Nothing -> readCmdTotal' namer withPid target >>= either haltLostPid showTotal
      Just spanSecs -> do
        let unfold = unfoldCmdTotalAfter' namer withPid spanSecs
        loopShowingTotals unfold target showTotal
    else case choiceWatchSecs cs of
      Nothing -> readCmdTotal' namer dropId target >>= either haltLostPid showTotal
      Just spanSecs -> do
        let unfold = unfoldCmdTotalAfter' namer dropId spanSecs
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


-- | Load the @'CmdTotal'@ corresponding to given @'Target'@ after a delay in seconds.
unfoldCmdTotalAfter ::
  (Integral p) =>
  p ->
  Target ->
  IO ([ProcessID], Maybe (Map ProcName CmdTotal, Target))
unfoldCmdTotalAfter = unfoldCmdTotalAfter' nameFor dropId


unfoldCmdTotalAfter' ::
  (Ord a, Integral p) =>
  (ProcessID -> IO (Either LostPid ProcName)) ->
  ((ProcessID, ProcName, PerProc) -> (a, PerProc)) ->
  p ->
  Target ->
  IO ([ProcessID], Maybe (Map a CmdTotal, Target))
unfoldCmdTotalAfter' namer mkCmd spanSecs target = do
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


-- | Load the @'CmdTotal'@ corresponding to the given @pid@
readPidTotal :: ProcessID -> IO (Either LostPid (ProcName, CmdTotal))
readPidTotal pid = do
  let onePid = pid :| []
      orNoProc = maybe (Left $ NoProc pid) Right . Map.lookupMin
  mkTarget onePid >>= \case
    Left _ -> pure $ Left $ NoProc pid
    Right target -> do
      readCmdTotal target >>= \case
        Right x -> pure $ orNoProc x
        Left err -> pure $ Left err


-- | Load the @'CmdTotal'@ corresponding to given @'Target'@
readCmdTotal :: Target -> IO (Either LostPid (Map ProcName CmdTotal))
readCmdTotal = readCmdTotal' nameFor dropId


readCmdTotal' ::
  Ord a =>
  (ProcessID -> IO (Either LostPid ProcName)) ->
  ((ProcessID, ProcName, PerProc) -> (a, PerProc)) ->
  Target ->
  IO (Either LostPid (Map a CmdTotal))
readCmdTotal' namer mkCmd target = do
  let amass' cmds = amass (tHasPss target) $ map mkCmd cmds
  fmap amass' <$> foldlEitherM (readNameAndStats namer target) (tPids target)


readNameAndStats ::
  (ProcessID -> IO (Either LostPid ProcName)) ->
  Target ->
  ProcessID ->
  IO (Either LostPid (ProcessID, ProcName, PerProc))
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


verify :: Choices -> IO Target
verify cs = case choicePidsToShow cs of
  Just tPids -> do
    -- halt if any specified pid cannot be accessed
    checkAllExist tPids
    mkTarget tPids >>= either haltErr pure
  Nothing -> do
    -- if choicePidsToShow is Nothing, must be running as root
    isRoot' <- isRoot
    unless isRoot' $ haltErr "run as root when no pids are specified using -p"
    allKnownProcs >>= mkTarget >>= either haltErr pure


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


nameAsFullCmd :: ProcessID -> IO (Either LostPid ProcName)
nameAsFullCmd pid = do
  let cmdlinePath = pidPath "cmdline" pid
      err = NoCmdLine pid
      recombine = Text.intercalate " " . NE.toList
      orLostPid = maybe (Left err) (Right . recombine)
  readUtf8Text cmdlinePath >>= (pure . orLostPid) . parseCmdline


nameFromExeOnly :: ProcessID -> IO (Either LostPid ProcName)
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
  let smapPath = pidPath "smaps" pid
      rollupPath = pidPath "smaps_rollup" pid
  hasSmaps <- doesFileExist smapPath
  hasRollup <- doesFileExist rollupPath
  if
      | hasRollup -> readUtf8Text rollupPath
      | hasSmaps -> readUtf8Text smapPath
      | otherwise -> pure Text.empty


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


withPid :: (ProcessID, Text, PerProc) -> ((ProcessID, Text), PerProc)
withPid (pid, name, pp) = ((pid, name), pp)


dropId :: (ProcessID, Text, PerProc) -> (Text, PerProc)
dropId (_pid, name, pp) = (name, pp)
