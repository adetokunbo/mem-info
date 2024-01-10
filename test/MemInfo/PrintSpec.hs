{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : MemInfo.ProcSpec
Copyright   : (c) 2023 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3
-}
module MemInfo.PrintSpec (spec) where

import qualified Data.Text as Text
import System.MemInfo.Prelude
import System.MemInfo.Print (fmtAsHeader, fmtCmdTotal, fmtOverall)
import System.MemInfo.Proc (CmdTotal (..))
import Test.Hspec


spec :: Spec
spec = describe "module System.MemInfo.Print" $ do
  fmtAsHeaderSpec
  fmtOverallSpec
  fmtCmdTotalSpec


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


fmtCmdTotalSpec :: Spec
fmtCmdTotalSpec = describe "fmtCmdTotal" $ do
  describe "when swap is not required" $ do
    it "does not show it" $
      fmtCmdTotal False sampleName sampleTotal `shouldBe` sampleTotalNoSwap

  describe "when swap is required" $ do
    it "does not show" $
      fmtCmdTotal True sampleName sampleTotal `shouldBe` sampleTotalSwap


sampleName :: (ProcessID, Text)
sampleName = (100, "TestCommand")


sampleTotal :: CmdTotal
sampleTotal =
  CmdTotal
    { ctShared = 1
    , ctPrivate = 2
    , ctCount = 3
    , ctSwap = 4
    }


sampleTotalNoSwap :: Text
sampleTotalNoSwap = "   1.0 KiB +    1.0 KiB =    2.0 KiB\tTestCommand [100] (3)"


sampleTotalSwap :: Text
sampleTotalSwap = "   1.0 KiB +    1.0 KiB =    2.0 KiB   4.0 KiB\tTestCommand [100] (3)"
