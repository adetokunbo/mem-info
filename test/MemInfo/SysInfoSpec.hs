{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : MemInfo.SysInfoSpec
Copyright   : (c) 2023 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3
-}
module MemInfo.SysInfoSpec (spec) where

import Data.GenValidity (GenValid (..))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Word (Word16, Word8)
import Fmt (build, fmt, (+|), (|+))
import MemInfo.OrphanInstances ()
import MemInfo.ProcSpec (genSmapLine)
import System.Directory (createDirectoryIfMissing, listDirectory, removePathForcibly)
import System.FilePath (takeDirectory, (</>))
import System.IO.Temp (withSystemTempDirectory)
import System.MemInfo.SysInfo (
  KernelVersion,
  RamFlaw (..),
  ReportBud (..),
  SwapFlaw (..),
  mkReportBud,
  parseKernelVersion,
 )
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, pick, run)


spec :: Spec
spec = describe "module System.MemInfo.SysInfo" $ do
  describe "parseKernelVersion" $ do
    it "should parse values from Text successfully" prop_roundtripKernelVersion
  mkReportBudSpec


usingTmp :: (FilePath -> IO a) -> IO a
usingTmp = withSystemTempDirectory "procSpec"


mkReportBudSpec :: Spec
mkReportBudSpec = do
  describe "mkReportBud" $ around usingTmp $ do
    context "when the kernel is 2.4.x" $ do
      context "and there is no Inact_ ram" $ do
        it "generate a ReportBud" prop_NonInactRamFlawOn24Kernel
      context "and there is Inact_ ram" $ do
        it "generate a ReportBud" prop_InactRamFlowOn24Kernel


prop_roundtripKernelVersion :: Property
prop_roundtripKernelVersion =
  within 5000000 $
    forAll genOsRelease $
      \(version, txt) -> Just version == parseKernelVersion txt


genOsRelease :: Gen (KernelVersion, Text)
genOsRelease = oneof [fromSingle, fromDouble, fromTriple]


fromTriple :: Gen (KernelVersion, Text)
fromTriple = do
  let toN = fromIntegral
      txt a b c = "" +| a |+ "." +| b |+ "." +| c |+ ""
  (x, y, z) <- genValid :: Gen (Word8, Word8, Word8)
  zSuffixed <- someWithSuffix z
  pure ((toN x, toN y, toN z), txt x y zSuffixed)


fromDouble :: Gen (KernelVersion, Text)
fromDouble = do
  (x, y) <- genValid :: Gen (Word8, Word8)
  ySuffixed <- someWithSuffix y
  let toN = fromIntegral
      txt a b = "" +| a |+ "." +| b |+ ""
  pure ((toN x, toN y, 0), txt x ySuffixed)


suffixes :: [Text]
suffixes = ["-pre", "b", "-kali", "-test"]


someWithSuffix :: Word8 -> Gen Text
someWithSuffix w = do
  addSuffix <- genValid
  s <- elements suffixes
  if addSuffix
    then pure $ "" +| w |+ "" +| s |+ ""
    else pure $ fmt $ build w


fromSingle :: Gen (KernelVersion, Text)
fromSingle = do
  x <- genValid :: Gen Word8
  pure ((fromIntegral x, 0, 0), fmt $ build x)


prop_NonInactRamFlawOn24Kernel :: FilePath -> Property
prop_NonInactRamFlawOn24Kernel = prop_With24Kernel True


prop_InactRamFlowOn24Kernel :: FilePath -> Property
prop_InactRamFlowOn24Kernel = prop_With24Kernel False


prop_With24Kernel :: Bool -> FilePath -> Property
prop_With24Kernel hasInactRam root = monadicIO $ do
  thePid <- pick genValidProcId
  version <- pick gen24Kernel
  memInfoTxt <- pick $ genMemInfoLines hasInactRam
  -- (_ignored, smapsTxt) <- pick genBaseSmap
  bud <- run $ do
    initProcDir root version
    writeRootedFile root "meminfo" memInfoTxt
    -- writeRootedFile root ("" +| thePid |+ "/smaps") smapsTxt
    mkReportBud root (fromIntegral thePid :| [])
  let want' = expectedBud thePid root version
      wantRamFlaw = if hasInactRam then SomeSharedMem else ExactForIsolatedMem
      want =
        want'
          { rbRamFlaws = Just wantRamFlaw
          , rbSwapFlaws = Just NoSwap
          }
  assert (bud == Just want)


genValidProcId :: Gen Word16
genValidProcId = genValid `suchThat` (> 1)


gen24Kernel :: Gen KernelVersion
gen24Kernel = do
  patch <- genValid
  pure (2, 4, patch)


genMemInfoLines :: Bool -> Gen Text
genMemInfoLines hasInact = do
  -- reference: http://darenmatthews.com/blog/?p=2092
  (_unused1, memTotalTxt) <- genSmapLine "MemTotal"
  (_unused2, memFreeTxt) <- genSmapLine "MemFree"
  (_unused3, buffersTxt) <- genSmapLine "Buffers"
  (_unused4, inactTxt) <- genSmapLine "Inact_Target" -- or Inact_{Clean,Dirty}
  let baselines = [memTotalTxt, memFreeTxt, buffersTxt]
      ls = if hasInact then baselines <> [inactTxt] else baselines
  pure $ Text.unlines ls


expectedBud :: Word16 -> FilePath -> KernelVersion -> ReportBud
expectedBud onePid rbProcRoot rbKernel =
  ReportBud
    { rbPids = fromIntegral onePid :| []
    , rbKernel
    , rbHasPss = False
    , rbHasSwapPss = False
    , rbHasSmaps = False
    , rbRamFlaws = Nothing
    , rbSwapFlaws = Nothing
    , rbProcRoot
    }


fmtKernelVersion :: KernelVersion -> Text
fmtKernelVersion (major, minor, patch) =
  "" +| toInteger major |+ "." +| toInteger minor |+ "." +| toInteger patch |+ ""


initProcDir :: FilePath -> KernelVersion -> IO ()
initProcDir root version = do
  clearDirectory root
  writeRootedFile root "sys/kernel/osrelease" $ fmtKernelVersion version


writeRootedFile :: FilePath -> FilePath -> Text -> IO ()
writeRootedFile root path txt = do
  let target = root </> path
  createDirectoryIfMissing True $ takeDirectory target
  Text.writeFile target txt


clearDirectory :: FilePath -> IO ()
clearDirectory fp = listDirectory fp >>= mapM_ removePathForcibly
