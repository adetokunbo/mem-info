{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : System.MemInfo.Proc
Copyright   : (c) 2022 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3

This module provides data types that model data from files in the @proc@
filesystem that track memory usage metrics, and combinators for parsing and
grouping the contents of those files.
-}
module System.MemInfo.Proc (
  -- * data types
  PerProc (..),
  CmdTotal (..),

  -- * functions
  amass,
  parseFromSmap,
  parseFromStatm,
) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import System.MemInfo.SysInfo (KernelVersion, unknownShared)
import System.Process.CoreMem.Prelude


-- | Combine @'PerProc'@ metrics grouped by command name
amass ::
  Ord a =>
  Bool ->
  [(a, PerProc)] ->
  Map a CmdTotal
amass hasPss = Map.map (fromSubTotal hasPss) . foldl' (incrSubTotals hasPss) mempty


fromSubTotal :: Bool -> SubTotal -> CmdTotal
fromSubTotal hasPss st =
  let reducedPrivate = stPrivate st `div` stCount st
      areThreads = threadsNotProcs st
      reducedShared = stShared st `div` stCount st
      newPrivate = if areThreads then reducedPrivate else stPrivate st
      newShared = if areThreads && hasPss then reducedShared else stShared st
   in CmdTotal
        { ctShared = newShared + stSharedHuge st
        , ctPrivate = newPrivate + newShared + stSharedHuge st
        , ctSwap = stSwap st
        , ctCount = stCount st
        }


-- | Represents the memory totals for each command
data CmdTotal = CmdTotal
  { ctShared :: !Int
  , ctPrivate :: !Int
  , ctCount :: !Int
  , ctSwap :: !Int
  }
  deriving (Eq, Show)


incrSubTotals ::
  Ord a =>
  Bool ->
  Map a SubTotal ->
  (a, PerProc) ->
  Map a SubTotal
incrSubTotals hasPss acc (cmd, mem) =
  let combinePrivate next prev | hasPss = next + prev
      combinePrivate next prev = max next prev
      nextSt =
        SubTotal
          { stShared = ppShared mem
          , stSharedHuge = ppSharedHuge mem
          , stCount = 1
          , stPrivate = ppPrivate mem
          , stSwap = ppSwap mem
          , stMemIds = Set.singleton $ ppMemId mem
          }
      update' next prev =
        prev
          { stShared = stShared next + stShared prev
          , stSharedHuge = max (stSharedHuge next) (stSharedHuge prev)
          , stPrivate = combinePrivate (stPrivate next) (stPrivate prev)
          , stCount = stCount next + stCount prev
          , stSwap = stSwap next + stSwap prev
          , stMemIds = Set.union (stMemIds next) (stMemIds prev)
          }
   in Map.insertWith update' cmd nextSt acc


data SubTotal = SubTotal
  { stShared :: !Int
  , stSharedHuge :: !Int
  , stPrivate :: !Int
  , stCount :: !Int
  , stSwap :: !Int
  , stMemIds :: !(Set Int)
  }
  deriving (Eq, Show)


-- If a process is invoked with clone using flags CLONE_VM and not CLONE_THREAD
-- it will share the same memory space as it's parent; this needs to accounted
-- for
--
-- This is detected by computing the memId has the hash of lines for the proc
-- read from its smaps file.
threadsNotProcs :: SubTotal -> Bool
threadsNotProcs cs = Set.size (stMemIds cs) == 1 && stCount cs > 1


-- | Represents the memory metrics for a single process
data PerProc = PerProc
  { ppPrivate :: !Int
  , ppShared :: !Int
  , ppSharedHuge :: !Int
  , ppSwap :: !Int
  , ppMemId :: !Int
  }
  deriving (Eq, Show)


-- value used as page size when @MemStat@ is calcuated from statm
pageSizeKiB :: Int
pageSizeKiB = 4


-- | Parse @'PerProc'@ from the contents of @/proc/<pid>/statm@
parseFromStatm :: KernelVersion -> Text -> Maybe PerProc
parseFromStatm version content =
  let
    noShared = unknownShared version
    parseWord w (Just acc) = (\x -> Just (x : acc)) =<< readMaybe (Text.unpack w)
    parseWord _ Nothing = Nothing
    parseMetrics = foldr parseWord (Just mempty)
    withMemId = ppZero {ppMemId = hash content}
    fromRss rss _shared
      | noShared = withMemId {ppPrivate = rss * pageSizeKiB}
    fromRss rss shared =
      withMemId
        { ppShared = shared * pageSizeKiB
        , ppPrivate = (rss - shared) * pageSizeKiB
        }
   in
    case parseMetrics $ Text.words content of
      Just (_size : rss : shared : _xs) -> Just $ fromRss rss shared
      _ -> Nothing


ppZero :: PerProc
ppZero =
  PerProc
    { ppPrivate = 0
    , ppShared = 0
    , ppSharedHuge = 0
    , ppSwap = 0
    , ppMemId = 0
    }


-- | Parse @'PerProc'@ from the contents of @/proc/<pid>/smap@
parseFromSmap :: Text -> PerProc
parseFromSmap = fromSmap . parseSmapStats


parseSmapStats :: Text -> SmapStats
parseSmapStats content =
  let noMemId = foldl' incrSmapStats ssZero $ Text.lines content
   in noMemId {ssMemId = hash content}


fromSmap :: SmapStats -> PerProc
fromSmap ss =
  let pssTweak = ssPssCount ss `div` 2 -- add ~0.5 per line to counter truncation
      pssShared = ssPss ss + pssTweak - ssPrivate ss
   in PerProc
        { ppSwap = if ssHasSwapPss ss then ssSwapPss ss else ssSwap ss
        , ppShared = if ssHasPss ss then pssShared else ssShared ss
        , ppSharedHuge = ssSharedHuge ss
        , ppPrivate = ssPrivate ss + ssPrivateHuge ss
        , ppMemId = ssMemId ss
        }


-- | Represents per-process data read from  @PROC_ROOT@/smaps
data SmapStats = SmapStats
  { ssPss :: !Int
  , ssPssCount :: !Int
  , ssSwap :: !Int
  , ssSwapPss :: !Int
  , ssPrivate :: !Int
  , ssPrivateHuge :: !Int
  , ssSharedHuge :: !Int
  , ssShared :: !Int
  , ssMemId :: !Int
  , ssHasPss :: !Bool
  , ssHasSwapPss :: !Bool
  }
  deriving (Eq, Show)


ssZero :: SmapStats
ssZero =
  SmapStats
    { ssPss = 0
    , ssPssCount = 0
    , ssSwap = 0
    , ssSwapPss = 0
    , ssPrivate = 0
    , ssPrivateHuge = 0
    , ssSharedHuge = 0
    , ssShared = 0
    , ssHasSwapPss = False
    , ssHasPss = False
    , ssMemId = 0
    }


-- Q: is it worth the dependency to replace this with lens from a lens package ?
incrPss
  , incrSwap
  , incrSwapPss
  , incrPrivate
  , incrPrivateHuge
  , incrShared
  , incrSharedHuge ::
    SmapStats -> Maybe Int -> SmapStats
incrPss ms = maybe ms $ \n -> ms {ssPss = n + ssPss ms}
incrSwap ms = maybe ms $ \n -> ms {ssSwap = n + ssSwap ms}
incrSwapPss ms = maybe ms $ \n -> ms {ssSwapPss = n + ssSwapPss ms}
incrPrivate ms = maybe ms $ \n -> ms {ssPrivate = n + ssPrivate ms}
incrShared ms = maybe ms $ \n -> ms {ssShared = n + ssShared ms}
incrPrivateHuge ms = maybe ms $ \n -> ms {ssPrivateHuge = n + ssPrivateHuge ms}
incrSharedHuge ms = maybe ms $ \n -> ms {ssSharedHuge = n + ssSharedHuge ms}


incrSmapStats :: SmapStats -> Text -> SmapStats
incrSmapStats acc l =
  if
      | Text.isPrefixOf "Private_Hugetlb:" l -> incrPrivateHuge acc $ smapValMb l
      | Text.isPrefixOf "Shared_Hugetlb:" l -> incrSharedHuge acc $ smapValMb l
      | Text.isPrefixOf "Shared" l -> incrShared acc $ smapValMb l
      | Text.isPrefixOf "Private" l -> incrPrivate acc $ smapValMb l
      | Text.isPrefixOf "Pss:" l ->
          let acc' = acc {ssHasPss = True, ssPssCount = 1 + ssPssCount acc}
           in incrPss acc' $ smapValMb l
      | Text.isPrefixOf "Swap:" l -> incrSwap acc $ smapValMb l
      | Text.isPrefixOf "SwapPss:" l -> incrSwapPss (acc {ssHasSwapPss = True}) $ smapValMb l
      | otherwise -> acc


smapValMb :: Read a => Text -> Maybe a
smapValMb l =
  let memWords = Text.words l
      readVal (_ : x : _) = readMaybe $ Text.unpack x
      readVal _ = Nothing
   in readVal memWords
