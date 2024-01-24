{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : MemInfo.PrintSpec
Copyright   : (c) 2023 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3
-}
module MemInfo.PrintSpec (spec) where

import Data.Text (Text)
import qualified Data.Text as Text
import System.MemInfo.Print (fmtAsHeader, fmtMemUsage, fmtOverall)
import System.MemInfo.Proc (MemUsage (..))
import System.Posix.Types (ProcessID)
import Test.Hspec


spec :: Spec
spec = describe "module System.MemInfo.Print" $ do
  fmtAsHeaderSpec
  fmtOverallSpec
  fmtMemUsageSpec


fmtAsHeaderSpec :: Spec
fmtAsHeaderSpec = describe "fmtAsHeader" $ do
  describe "when swap is not required" $ do
    it "does not show it" $
      fmtAsHeader False `shouldBe` headerNoSwap

  describe "when swap is required" $ do
    it "does show it" $
      fmtAsHeader True `shouldBe` headerSwap


headerNoSwap :: Text
headerNoSwap = "  Private  +   Shared   =   RAM Used\tProgram   "


headerSwap :: Text
headerSwap = "  Private  +   Shared   =   RAM Used Swap Used\tProgram   "


fmtOverallSpec :: Spec
fmtOverallSpec = describe "fmtOverall" $ do
  describe "when swap is not required" $ do
    it "does not show it" $
      fmtOverall False (1, 1) `shouldBe` sampleNoSwapOverall
  describe "when swap is required" $ do
    it "does show it" $
      fmtOverall True (1, 1) `shouldBe` sampleSwapOverall


sampleNoSwapOverall :: Text
sampleNoSwapOverall =
  Text.unlines
    [ "------------------------------------"
    , "                             1.0 KiB"
    , "===================================="
    ]


sampleSwapOverall :: Text
sampleSwapOverall =
  Text.unlines
    [ "----------------------------------------------"
    , "                             1.0 KiB   1.0 KiB"
    , "=============================================="
    ]


fmtMemUsageSpec :: Spec
fmtMemUsageSpec = describe "fmtMemUsage" $ do
  describe "when displaying by-pid" $ do
    let usage = sampleUsage' 1
    describe "and swap is not required" $ do
      it "does not show it" $
        fmtMemUsage False biName usage `shouldBe` sampleTotalNoSwap

    describe "when swap is required" $ do
      it "shows it" $
        fmtMemUsage True biName usage `shouldBe` sampleTotalSwap

  describe "when displaying by-name" $ do
    let usage = sampleUsage' 3
    describe "and swap is not required" $ do
      it "does not show it" $
        fmtMemUsage False monoName usage `shouldBe` sampleTotalNoSwapMono


monoName :: Text
monoName = "by-name-cmd"


biName :: (ProcessID, Text)
biName = (100, "by-id-and-name-cmd")


sampleUsage' :: Int -> MemUsage
sampleUsage' muCount =
  MemUsage
    { muShared = 1
    , muPrivate = 2
    , muCount
    , muSwap = 4
    }


sampleTotalNoSwapMono :: Text
sampleTotalNoSwapMono = "   1.0 KiB +    1.0 KiB =    2.0 KiB\tby-name-cmd (3)"


sampleTotalNoSwap :: Text
sampleTotalNoSwap = "   1.0 KiB +    1.0 KiB =    2.0 KiB\tby-id-and-name-cmd [100] (1)"


sampleTotalSwap :: Text
sampleTotalSwap = "   1.0 KiB +    1.0 KiB =    2.0 KiB   4.0 KiB\tby-id-and-name-cmd [100] (1)"
