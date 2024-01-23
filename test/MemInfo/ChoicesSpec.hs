{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : MemInfo.ChoicesSpec
Copyright   : (c) 2023 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3
-}
module MemInfo.ChoicesSpec where

import Data.GenValidity (GenValid (..))
import qualified Data.List.NonEmpty as NE
import MemInfo.OrphanInstances ()
import Options.Applicative (defaultPrefs, execParserPure, getParseResult)
import System.MemInfo.Choices (Choices (..), cmdInfo)
import Test.Hspec
import Test.QuickCheck (Gen, Property, forAll, suchThat)


spec :: Spec
spec = describe "module System.MemInfo.Choices" $ do
  it "should parse a Choices from the command line ok" prop_roundtripParseChoices


prop_roundtripParseChoices :: Property
prop_roundtripParseChoices =
  forAll genCmdLine $
    \(choices, args) -> Just choices == getParseResult (execParserPure defaultPrefs cmdInfo args)


genCmdLine :: Gen (Choices, [String])
genCmdLine = do
  choices <- genValid `suchThat` ((/= Just 0) . choiceWatchSecs)
  pure (choices, cmdlineOf choices)


cmdlineOf :: Choices -> [String]
cmdlineOf c =
  let
    splitArgs = if choiceSplitArgs c then ("-s" :) else id
    onlyTotal = if choiceOnlyTotal c then ("-t" :) else id
    byPid = if choiceByPid c then ("-d" :) else id
    showSwap = if choiceShowSwap c then ("-S" :) else id
    watchSecs = maybe id (\x -> (("-w " ++ show x) :)) $ choiceWatchSecs c
    onePid x = "-p " ++ show x
    manyPids xs = (map onePid (NE.toList xs) ++)
    pidsToShow = maybe id manyPids $ choicePidsToShow c
   in
    pidsToShow $ splitArgs $ onlyTotal $ byPid $ showSwap $ watchSecs mempty
