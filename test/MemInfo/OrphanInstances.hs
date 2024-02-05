{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK prune not-home #-}

{- |
Module      : MemInfo.OrphanInstances
Copyright   : (c) 2022 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3
-}
module MemInfo.OrphanInstances where

import Data.GenValidity (GenValid (..))
import Data.GenValidity.Text ()
import Data.List.NonEmpty (nonEmpty)
import System.MemInfo.Choices (Choices (..), PrintOrder, Style)
import System.MemInfo.Proc (ExeInfo (..), StatusInfo)
import System.Posix.Types (CPid (..), ProcessID)
import Test.QuickCheck (Gen, frequency, suchThat)
import Test.QuickCheck.Gen (listOf)
import Test.Validity (Validity)


instance GenValid ExeInfo where
  genValid = do
    eiDeleted <- genValid
    eiOriginal <- genValid
    let eiTarget = if eiDeleted then eiOriginal <> " (deleted)" else eiOriginal
    pure $ ExeInfo {eiDeleted, eiOriginal, eiTarget}


genPositive :: (GenValid a, Num a, Ord a) => Gen a
genPositive = genValid `suchThat` (> 0)


deriving anyclass instance GenValid StatusInfo


deriving newtype instance Validity ProcessID


deriving newtype instance GenValid ProcessID


deriving instance Validity Choices


deriving instance Validity PrintOrder


deriving anyclass instance GenValid PrintOrder


deriving instance Validity Style


deriving instance GenValid Style


instance GenValid Choices where
  genValid =
    let genPositiveMb = frequency [(1, pure Nothing), (5, Just <$> genPositive)]
        genPids = nonEmpty <$> listOf genPositive
     in Choices
          <$> genValid
          <*> genValid
          <*> genValid
          <*> genValid
          <*> genValid
          <*> genPositiveMb
          <*> genPids
          <*> genValid
          <*> genValid
