{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Process.CoreMemSpec
Copyright   : (c) 2023 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3
-}
module Process.CoreMemSpec (spec) where

import Test.Hspec
import System.Process.CoreMem

spec :: Spec
spec = describe "CoreMem" $ do
  context "endsThen" $
    it "should be a simple test" $ do
      getIt `endsThen` (== (Just "a string"))


getIt :: IO (Maybe String)
getIt = pure $ Just "a string"
