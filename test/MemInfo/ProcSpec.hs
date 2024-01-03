{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : MemInfo.ProcSpec
Copyright   : (c) 2023 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3
-}
module MemInfo.ProcSpec (spec) where

import System.MemInfo.Proc
import Test.Hspec


spec :: Spec
spec = describe "Proc" $ do
  context "endsThen" $
    it "should be a simple test" $ do
      getIt `shouldReturn` Just "a string"


getIt :: IO (Maybe String)
getIt = pure $ Just "a string"
