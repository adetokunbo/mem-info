{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : System.MemInfo.SysInfo
Copyright   : (c) 2022 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3

This module provides information about the system on which the memory is being
measured.

@'readKernelVersion'@ determines the system's linux @'KernelVersion'@
-}
module System.MemInfo.SysInfo (
  -- * data types
  KernelVersion,
  Target (..),
  RamFlaw (..),
  SwapFlaw (..),

  -- * functions
  mkTarget,
  checkForFlaws,
  fmtRamFlaw,
  fmtSwapFlaw,
  fickleSharing,
  parseKernelVersion,
  readKernelVersion,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Read as Text
import Fmt ((+|), (|+))
import System.MemInfo.Prelude


-- | Represents a version of @Linux@ kernel
type KernelVersion = (Natural, Natural, Natural)


{- | On linux kernels before smaps became available, there was no reliable way to
determine how much of a processes memory was shared

http://lkml.org/lkml/2005/7/6/250
-}
fickleSharing :: KernelVersion -> Bool
fickleSharing k = k >= (2, 6, 1) && k <= (2, 6, 9)


-- | Determines the version of the Linux kernel on the current system.
readKernelVersion :: IO (Either Text KernelVersion)
readKernelVersion = parseKernelVersion <$> Text.readFile kernelVersionPath


kernelVersionPath :: String
kernelVersionPath = "/proc/sys/kernel/osrelease"


parseKernelVersion :: Text -> Either Text KernelVersion
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


-- | Gathers the inputs needed to generate a memory usage report
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


mkTarget :: NonEmpty ProcessID -> IO (Either Text Target)
mkTarget tPids = do
  let firstPid = NE.head tPids
      smapsPath = pidPath "smaps" firstPid
      hasPss = Text.isInfixOf "Pss:"
      hasSwapPss = Text.isInfixOf "SwapPss:"
      memtypes x = (hasPss x, hasSwapPss x)
  tHasSmaps <- doesFileExist smapsPath
  (tHasPss, tHasSwapPss) <- memtypes <$> readUtf8Text smapsPath
  readKernelVersion >>= \case
    Left err -> pure $ Left err
    Right tKernel ->
      fmap Right $
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


pidPath :: String -> ProcessID -> FilePath
pidPath base pid = "/proc/" +| toInteger pid |+ "/" +| base |+ ""
