{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : System.MemInfo.SysInfo
Copyright   : (c) 2022 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3

This module provides information about the system on where the memory is
measured.

@'readKernelVersion'@ determines the system's linux @'KernelVersion'@
-}
module System.MemInfo.SysInfo (
  -- * data types
  KernelVersion,

  -- * functions
  unknownShared,
  parseKernelVersion,
  readKernelVersion,
) where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Read as Text
import Numeric.Natural (Natural)


-- | Represents a version of @Linux@ kernel
type KernelVersion = (Natural, Natural, Natural)


{- | On linux kernels before smaps became available, there was no reliable way to
determine how much of a processes memory was shared

http://lkml.org/lkml/2005/7/6/250
-}
unknownShared :: KernelVersion -> Bool
unknownShared k = k >= (2, 6, 1) && k <= (2, 6, 9)


-- | Determines the version of the Linux kernel on the current system.
readKernelVersion :: IO (Either Text KernelVersion)
readKernelVersion = parseKernelVersion <$> Text.readFile kernelVersionPath


kernelVersionPath :: String
kernelVersionPath = "/proc/sys/kernel/osrelease"


parseKernelVersion :: Text -> Either Text KernelVersion
parseKernelVersion =
  let unrecognized = Left "unrecognized kernel version"
      dec' (Right (x, extra)) | Text.null extra = Right x
      dec' _ = unrecognized
      dec1st' (Right (x, _)) = Right x
      dec1st' _ = unrecognized

      dec = dec' . Text.decimal
      dec1st = dec1st' . Text.decimal
      fromSplit [x] = (,,) <$> dec x <*> pure 0 <*> pure 0
      fromSplit [x, y] = (,,) <$> dec x <*> dec1st y <*> pure 0
      fromSplit [x, y, z] = (,,) <$> dec x <*> dec y <*> dec1st z
      fromSplit _ = unrecognized
   in fromSplit . Text.split (== '.')
