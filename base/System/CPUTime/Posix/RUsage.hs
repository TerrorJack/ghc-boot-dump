{-# LINE 1 "System/CPUTime/Posix/RUsage.hsc" #-}
{-# LANGUAGE CPP, CApiFFI, NumDecimals #-}




module System.CPUTime.Posix.RUsage
    ( getCPUTime
    , getCpuTimePrecision
    ) where

import Data.Ratio
import Foreign
import Foreign.C
import System.CPUTime.Utils

-- For struct rusage

{-# LINE 18 "System/CPUTime/Posix/RUsage.hsc" #-}


{-# LINE 20 "System/CPUTime/Posix/RUsage.hsc" #-}

getCPUTime :: IO Integer
getCPUTime = allocaBytes (272) $ \ p_rusage -> do
{-# LINE 23 "System/CPUTime/Posix/RUsage.hsc" #-}
    throwErrnoIfMinus1_ "getrusage" $ getrusage (0) p_rusage
{-# LINE 24 "System/CPUTime/Posix/RUsage.hsc" #-}

    let ru_utime = ((\hsc_ptr -> hsc_ptr `plusPtr` 0)) p_rusage
{-# LINE 26 "System/CPUTime/Posix/RUsage.hsc" #-}
    let ru_stime = ((\hsc_ptr -> hsc_ptr `plusPtr` 16)) p_rusage
{-# LINE 27 "System/CPUTime/Posix/RUsage.hsc" #-}
    u_sec  <- ((\hsc_ptr -> peekByteOff hsc_ptr 0))  ru_utime :: IO CTime
{-# LINE 28 "System/CPUTime/Posix/RUsage.hsc" #-}
    u_usec <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) ru_utime :: IO CSUSeconds
{-# LINE 29 "System/CPUTime/Posix/RUsage.hsc" #-}
    s_sec  <- ((\hsc_ptr -> peekByteOff hsc_ptr 0))  ru_stime :: IO CTime
{-# LINE 30 "System/CPUTime/Posix/RUsage.hsc" #-}
    s_usec <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) ru_stime :: IO CSUSeconds
{-# LINE 31 "System/CPUTime/Posix/RUsage.hsc" #-}
    let usec = cTimeToInteger u_sec * 1e6 + csuSecondsToInteger u_usec +
               cTimeToInteger s_sec * 1e6 + csuSecondsToInteger s_usec
    return (usec * 1e6)

type CRUsage = ()
foreign import capi unsafe "HsBase.h getrusage" getrusage :: CInt -> Ptr CRUsage -> IO CInt

getCpuTimePrecision :: IO Integer
getCpuTimePrecision =
    return $ round ((1e12::Integer) % fromIntegral clk_tck)

foreign import ccall unsafe clk_tck :: CLong
