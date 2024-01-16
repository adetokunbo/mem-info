{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{- |
Module      : MemInfo.SysInfoSpec
Copyright   : (c) 2023 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3
-}
module MemInfo.SysInfoSpec (spec) where

import Data.GenValidity (GenValid (..))
import Data.Text (Text)
import Data.Word (Word8)
import Fmt (build, fmt, (+|), (|+))
import MemInfo.OrphanInstances ()
import System.MemInfo.SysInfo (KernelVersion, parseKernelVersion)
import Test.Hspec
import Test.QuickCheck


spec :: Spec
spec = describe "module System.MemInfo.SysInfo" $ do
  describe "parseKernelVersion" $ do
    it "should parse values from Text successfully" prop_roundtripKernelVersion


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
