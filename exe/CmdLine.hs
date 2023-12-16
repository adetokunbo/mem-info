{- |
Module      : CmdLine
Copyright   : (c) 2022 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3
-}
module Main (main) where

import System.Process.CoreMem (getChoices, printProcs)


main :: IO ()
main = getChoices >>= printProcs
