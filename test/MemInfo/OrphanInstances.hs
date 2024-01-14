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
import System.MemInfo.Prelude
import System.MemInfo.Proc (ExeInfo (..), StatusInfo)
import Test.Validity (Validity)


instance GenValid ExeInfo where
  genValid = do
    eiDeleted <- genValid
    eiOriginal <- genValid
    let eiTarget = if eiDeleted then eiOriginal <> " (deleted)" else eiOriginal
    pure $ ExeInfo {eiDeleted, eiOriginal, eiTarget}


deriving anyclass instance GenValid StatusInfo


deriving newtype instance Validity ProcessID


deriving newtype instance GenValid ProcessID
