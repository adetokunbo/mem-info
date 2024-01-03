{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : System.MemInfo.Print
Copyright   : (c) 2022 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3

This module contains functions for formatting parts of the memory report
-}
module System.MemInfo.Print (
  -- * classes
  AsCmdName (..),

  -- * functions
  fmtAsHeader,
  fmtOverall,
  fmtCmdTotal,
) where

import qualified Data.Text as Text
import Fmt (
  fixedF,
  padBothF,
  padLeftF,
  (+|),
  (+||),
  (|+),
  (|++|),
  (||+),
 )
import System.MemInfo.Proc (CmdTotal (..))
import System.MemInfo.Prelude


{- | Generates the text of a row displaying the metrics for a single command in
the memory report
-}
fmtCmdTotal :: AsCmdName a => Bool -> a -> CmdTotal -> Text
fmtCmdTotal showSwap name ct =
  let
    padl = padLeftF columnWidth ' ' . fmtMem
    private = padl $ ctPrivate ct - ctShared ct
    shared = padl $ ctShared ct
    all' = padl $ ctPrivate ct
    swap' = padl $ ctSwap ct
    name' = cmdWithCount name $ ctCount ct
    ram = "" +| private |+ " + " +| shared |+ " = " +| all' |+ ""
    label = "" +| name' |+ ""
   in
    if showSwap
      then ram <> ("" +| swap' |+ "\t") <> label
      else ram <> "\t" <> label


-- | Generates the text showing the overall memory in the memory report
fmtOverall :: Bool -> (Int, Int) -> Text
fmtOverall showSwap (private, swap) =
  let
    rimLength = if showSwap then 46 else 36
    gapLength = 26
    top = Text.replicate rimLength "-"
    gap = Text.replicate gapLength " "
    bottom = Text.replicate rimLength "="
    padl = padLeftF columnWidth ' ' . fmtMem
    withSwap = "" +| gap |++| padl private |++| padl swap |+ ""
    noSwap = "" +| gap |++| padl private |+ ""
    out = if showSwap then withSwap else noSwap
   in
    Text.unlines [top, out, bottom]


data Power = Ki | Mi | Gi | Ti deriving (Eq, Show, Ord, Enum, Bounded)


fmtMem :: Int -> Text
fmtMem = fmtMem' Ki . fromIntegral


columnWidth :: Int
columnWidth = 10


fmtMem' :: Power -> Float -> Text
fmtMem' =
  let doFmt p x = "" +| fixedF 1 x |+ " " +|| p ||+ "B"
      go p x | p == maxBound = doFmt p x
      go p x | x > 1000 = fmtMem' (succ p) (x / 1024)
      go p x = doFmt p x
   in go


hdrPrivate, hdrShared, hdrRamUsed, hdrSwapUsed, hdrProgram :: Text
hdrPrivate = "Private"
hdrShared = "Shared"
hdrRamUsed = "RAM Used"
hdrSwapUsed = "Swap Used"
hdrProgram = "Program"


-- | Generates the text of the printed header of the memory report
fmtAsHeader :: Bool -> Text
fmtAsHeader showSwap =
  let
    padb = padBothF columnWidth ' '
    private = padb hdrPrivate
    shared = padb hdrShared
    all' = padb hdrRamUsed
    name' = padb hdrProgram
    swap' = padb hdrSwapUsed
    ram = "" +| private |+ " + " +| shared |+ " = " +| all' |+ ""
    label = "" +| name' |+ ""
   in
    if showSwap
      then ram <> ("" +| swap' |+ "\t") <> label
      else ram <> "\t" <> label


cmdWithCount :: AsCmdName a => a -> Int -> Text
cmdWithCount cmd count = "" +| asCmdName cmd |+ " (" +| count |+ ")"


-- | Represents a label that is as the command name in the report output
class AsCmdName a where
  -- Convert the label to text for output
  asCmdName :: a -> Text


instance AsCmdName Text where
  asCmdName = id


instance AsCmdName (ProcessID, Text) where
  asCmdName (pid, name) = "" +| name |+ " [" +| toInteger pid |+ "]"
