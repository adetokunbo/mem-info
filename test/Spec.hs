{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified MemInfo.PrintSpec as Print
import qualified MemInfo.ProcSpec as Proc
import qualified MemInfo.SysInfoSpec as SysInfo
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
    Print.spec
    SysInfo.spec
