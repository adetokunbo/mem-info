{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified MemInfo.ProcSpec as Proc
import System.IO (
  BufferMode (..),
  hSetBuffering,
  stderr,
  stdout,
 )
import Test.Hspec


main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering
  hspec $ do
    Proc.spec
