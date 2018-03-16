{-# LINE 1 "System/CPUTime/Posix/Times.hsc" #-}
{-# LANGUAGE CPP, CApiFFI, NumDecimals #-}




module System.CPUTime.Posix.Times
    ( getCPUTime
    , getCpuTimePrecision
    ) where

import Data.Ratio
import Foreign
import Foreign.C
import System.CPUTime.Utils

-- for struct tms

{-# LINE 18 "System/CPUTime/Posix/Times.hsc" #-}


{-# LINE 20 "System/CPUTime/Posix/Times.hsc" #-}

getCPUTime :: IO Integer
getCPUTime = allocaBytes (32) $ \ p_tms -> do
{-# LINE 23 "System/CPUTime/Posix/Times.hsc" #-}
    _ <- times p_tms
    u_ticks  <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) p_tms :: IO CClock
{-# LINE 25 "System/CPUTime/Posix/Times.hsc" #-}
    s_ticks  <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) p_tms :: IO CClock
{-# LINE 26 "System/CPUTime/Posix/Times.hsc" #-}
    return (( (cClockToInteger u_ticks + cClockToInteger s_ticks) * 1e12)
                        `div` fromIntegral clockTicks)

type CTms = ()
foreign import ccall unsafe times :: Ptr CTms -> IO CClock

getCpuTimePrecision :: IO Integer
getCpuTimePrecision =
    return $ round ((1e12::Integer) % clockTicks)

foreign import ccall unsafe clk_tck :: CLong

clockTicks :: Integer
clockTicks = fromIntegral clk_tck
