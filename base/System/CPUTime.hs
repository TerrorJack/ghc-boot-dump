{-# LINE 1 "System/CPUTime.hsc" #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP, CApiFFI #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  System.CPUTime
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- The standard CPUTime library.
--
-----------------------------------------------------------------------------




-- For various _POSIX_* #defines

{-# LINE 23 "System/CPUTime.hsc" #-}


{-# LINE 25 "System/CPUTime.hsc" #-}

module System.CPUTime
    ( getCPUTime
    , cpuTimePrecision
    ) where

import System.IO.Unsafe (unsafePerformIO)

-- Here is where we decide which backend to use

{-# LINE 38 "System/CPUTime.hsc" #-}
import qualified System.CPUTime.Posix.ClockGetTime as I


{-# LINE 55 "System/CPUTime.hsc" #-}

-- | The 'cpuTimePrecision' constant is the smallest measurable difference
-- in CPU time that the implementation can record, and is given as an
-- integral number of picoseconds.
cpuTimePrecision :: Integer
cpuTimePrecision = unsafePerformIO I.getCpuTimePrecision
{-# NOINLINE cpuTimePrecision #-}

-- | Computation 'getCPUTime' returns the number of picoseconds CPU time
-- used by the current program.  The precision of this result is
-- implementation-dependent.
getCPUTime :: IO Integer
getCPUTime = I.getCPUTime
