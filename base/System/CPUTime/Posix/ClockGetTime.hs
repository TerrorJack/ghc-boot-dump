{-# LINE 1 "System/CPUTime/Posix/ClockGetTime.hsc" #-}
{-# LANGUAGE CPP, CApiFFI, NumDecimals #-}




{-# LINE 6 "System/CPUTime/Posix/ClockGetTime.hsc" #-}



{-# LINE 9 "System/CPUTime/Posix/ClockGetTime.hsc" #-}

module System.CPUTime.Posix.ClockGetTime
    ( getCPUTime
    , getCpuTimePrecision
    ) where


{-# LINE 16 "System/CPUTime/Posix/ClockGetTime.hsc" #-}

import Foreign
import Foreign.C
import System.CPUTime.Utils

getCPUTime :: IO Integer
getCPUTime = fmap snd $ withTimespec $ \ts ->
    throwErrnoIfMinus1_ "clock_gettime"
    $ clock_gettime (2) ts
{-# LINE 25 "System/CPUTime/Posix/ClockGetTime.hsc" #-}

getCpuTimePrecision :: IO Integer
getCpuTimePrecision = fmap snd $ withTimespec $ \ts ->
    throwErrnoIfMinus1_ "clock_getres"
    $ clock_getres (2) ts
{-# LINE 30 "System/CPUTime/Posix/ClockGetTime.hsc" #-}

data Timespec

-- | Perform the given action to fill in a @struct timespec@, returning the
-- result of the action and the value of the @timespec@ in picoseconds.
withTimespec :: (Ptr Timespec -> IO a) -> IO (a, Integer)
withTimespec action =
    allocaBytes (16) $ \p_ts -> do
{-# LINE 38 "System/CPUTime/Posix/ClockGetTime.hsc" #-}
        r <- action p_ts
        u_sec  <- ((\hsc_ptr -> peekByteOff hsc_ptr 0))  p_ts :: IO CTime
{-# LINE 40 "System/CPUTime/Posix/ClockGetTime.hsc" #-}
        u_nsec <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) p_ts :: IO CLong
{-# LINE 41 "System/CPUTime/Posix/ClockGetTime.hsc" #-}
        return (r, cTimeToInteger u_sec * 1e12 + fromIntegral u_nsec * 1e3)

foreign import capi unsafe "time.h clock_getres"  clock_getres  :: CInt -> Ptr Timespec -> IO CInt
foreign import capi unsafe "time.h clock_gettime" clock_gettime :: CInt -> Ptr Timespec -> IO CInt


{-# LINE 56 "System/CPUTime/Posix/ClockGetTime.hsc" #-}
