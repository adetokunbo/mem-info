{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{- |
Module      : MemInfo.ProcSpec
Copyright   : (c) 2023 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3
-}
module MemInfo.ProcSpec (spec, genStatusInfoContent) where

import Data.Word (Word16)
import Fmt (blockMapF, build, fmt, (+|), (|+))
import MemInfo.OrphanInstances ()
import System.MemInfo.Prelude
import System.MemInfo.Proc
import Test.Hspec
import Test.QuickCheck
import Test.Validity (GenValid (..), forAllValid)
import Test.Validity.GenValidity (genValidSpec)


spec :: Spec
spec = describe "Proc" $ do
  exeInfoSpec
  statusInfoSpec
  fromStatmSpec


exeInfoSpec :: Spec
exeInfoSpec = describe "ExeInfo" $ do
  genValidSpec @ExeInfo
  it "should parse all valid values successfully" $ do
    forAllValid roundtripEI


roundtripEI :: ExeInfo -> Bool
roundtripEI ei = ei == parseExeInfo (eiTarget ei)


genOthers :: Gen [(Text, Word16)]
genOthers = do
  keys <- sublistOf otherStatusFields `suchThat` (not . null)
  vals <- vectorOf (length keys) arbitrary
  pure $ zip keys vals


statusInfoSpec :: Spec
statusInfoSpec = describe "StatusInfo" $ do
  it "should parse all valid values successfully" $ do
    forAll genStatusInfoContent $ uncurry roundtripSI


roundtripSI :: StatusInfo -> Text -> Bool
roundtripSI si txt = Right si == parseStatusInfo txt


genStatusInfo :: Gen StatusInfo
genStatusInfo = genValid


genProcStatus :: StatusInfo -> Gen [(Text, Text)]
genProcStatus status = do
  others <- fmap (fmap (\(x, y) -> (x, fmt $ build y))) genOthers
  pure $ others <> asFields status


genStatusInfoContent :: Gen (StatusInfo, Text)
genStatusInfoContent = do
  si <- genStatusInfo
  txt <- fmt . blockMapF <$> genProcStatus si
  pure (si, txt)


asFields :: StatusInfo -> [(Text, Text)]
asFields si =
  [ ("Name", siName si)
  , ("PPid", fmt $ build $ toInteger $ siParent si)
  ]


otherStatusFields :: [Text]
otherStatusFields = ["Uid", "Gid", "FDSize", "Ngid", "Threads", "Cpus_allowed"]


fromStatmSpec :: Spec
fromStatmSpec = describe "parseFromStatm" $ do
  describe "when using a kernel version with unknown sharing" $ do
    it "should parse values to PerProc successfully" $ do
      forAll genNoSharedStatm $ uncurry $ roundtripFromStatm badSharedKernel
  describe "when using a kernel version with known sharing" $ do
    it "should parse values to PerProc successfully" $ do
      forAll genSharedStatm $ uncurry $ roundtripFromStatm sharedKernel


statmNoShared :: Word16 -> Text
statmNoShared rss = "0 " +| toInteger rss |+ " 1 2 3 4"


roundtripFromStatm :: (Natural, Natural, Natural) -> PerProc -> Text -> Bool
roundtripFromStatm version pp txt = Just pp == parseFromStatm version txt


badSharedKernel :: (Natural, Natural, Natural)
badSharedKernel = (2, 6, 1)


sharedKernel :: (Natural, Natural, Natural)
sharedKernel = (2, 7, 1)


genNoSharedStatm :: Gen (PerProc, Text)
genNoSharedStatm = do
  rssKb <- genValid
  let content = statmNoShared rssKb
      pp =
        ppZero
          { ppPrivate = fromIntegral rssKb * pageSizeKiB
          , ppMemId = hash content
          }
  pure (pp, content)


genSharedStatm :: Gen (PerProc, Text)
genSharedStatm = do
  sharedKb <- genValid
  rssKb <- genValid `suchThat` (<) sharedKb
  let content = statmShared rssKb sharedKb
      pp =
        ppZero
          { ppPrivate = fromIntegral (rssKb - sharedKb) * pageSizeKiB
          , ppMemId = hash content
          , ppShared = fromIntegral sharedKb * pageSizeKiB
          }
  pure (pp, content)


statmShared :: Word16 -> Word16 -> Text
statmShared rss shared = "0 " +| toInteger rss |+ " " +| toInteger shared |+ " 1 2 3"


pageSizeKiB :: Int
pageSizeKiB = 4


ppZero :: PerProc
ppZero =
  PerProc
    { ppPrivate = 0
    , ppShared = 0
    , ppSharedHuge = 0
    , ppSwap = 0
    , ppMemId = 0
    }
