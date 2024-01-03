{- |
Module      : System.MemInfo.Prelude
Copyright   : (c) 2022 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3

Imports and re-export commonly-used functions and data types
-}
module System.MemInfo.Prelude (
  -- * functions
  isNull,
  isNullOrSpace,

  -- * module re-exports
  module Control.Concurrent,
  module Control.Exception,
  module Control.Monad,
  module Data.Char,
  module Data.Foldable,
  module Data.Hashable,
  module Data.List,
  module Data.List.NonEmpty,
  module Data.Maybe,
  module Data.Map.Strict,
  module Data.Set,
  module Data.Text,
  module Data.Text.Encoding,
  module Numeric.Natural,
  module System.FilePath,
  module System.IO.Error,
  module System.IO,
  module System.Posix.Types,
  module Text.Read,
) where

import Control.Concurrent (threadDelay)
import Control.Exception (handle, throwIO)
import Control.Monad (filterM, unless, when)
import Data.Char (isSpace)
import Data.Foldable (foldlM)
import Data.Hashable (hash)
import Data.List (foldl')
import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import Data.Map.Strict (Map)
import Data.Maybe (isJust, mapMaybe)
import Data.Set (Set)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Numeric.Natural (Natural)
import System.FilePath (takeBaseName)
import System.IO (stderr)
import System.IO.Error (isDoesNotExistError, isPermissionError)
import System.Posix.Types (ProcessID)
import Text.Read (readEither, readMaybe)


-- | @True@ for the @null@ char
isNull :: Char -> Bool
isNull = (== '\0')


-- | @True@ for the @null@ char or any space
isNullOrSpace :: Char -> Bool
isNullOrSpace x = isSpace x || isNull x
