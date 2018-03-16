{-# LINE 1 "GHC/Stats.hsc" #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

-----------------------------------------------------------------------------
-- | This module provides access to internal garbage collection and
-- memory usage statistics.  These statistics are not available unless
-- a program is run with the @-T@ RTS flag.
--
-- This module is GHC-only and should not be considered portable.
--
-- @since 4.5.0.0
-----------------------------------------------------------------------------
module GHC.Stats
    (
    -- * Runtime statistics
      RTSStats(..), GCDetails(..), RtsTime
    , getRTSStats
    , getRTSStatsEnabled
) where

import Control.Monad
import Data.Int
import Data.Word
import GHC.Base
import GHC.Read ( Read )
import GHC.Show ( Show )
import GHC.IO.Exception
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.Ptr



foreign import ccall "getRTSStats" getRTSStats_ :: Ptr () -> IO ()

-- | Returns whether GC stats have been enabled (with @+RTS -T@, for example).
--
-- @since 4.10.0.0
foreign import ccall "getRTSStatsEnabled" getRTSStatsEnabled :: IO Bool

--
-- | Statistics about runtime activity since the start of the
-- program.  This is a mirror of the C @struct RTSStats@ in @RtsAPI.h@
--
-- @since 4.10.0.0
--
data RTSStats = RTSStats {
  -- -----------------------------------
  -- Cumulative stats about memory use

    -- | Total number of GCs
    gcs :: Word32
    -- | Total number of major (oldest generation) GCs
  , major_gcs :: Word32
    -- | Total bytes allocated
  , allocated_bytes :: Word64
    -- | Maximum live data (including large objects + compact regions)
  , max_live_bytes :: Word64
    -- | Maximum live data in large objects
  , max_large_objects_bytes :: Word64
    -- | Maximum live data in compact regions
  , max_compact_bytes :: Word64
    -- | Maximum slop
  , max_slop_bytes :: Word64
    -- | Maximum memory in use by the RTS
  , max_mem_in_use_bytes :: Word64
    -- | Sum of live bytes across all major GCs.  Divided by major_gcs
    -- gives the average live data over the lifetime of the program.
  , cumulative_live_bytes :: Word64
    -- | Sum of copied_bytes across all GCs
  , copied_bytes :: Word64
    -- | Sum of copied_bytes across all parallel GCs
  , par_copied_bytes :: Word64
    -- | Sum of par_max_copied_bytes across all parallel GCs. Deprecated.
  , cumulative_par_max_copied_bytes :: Word64
    -- | Sum of par_balanced_copied bytes across all parallel GCs
  , cumulative_par_balanced_copied_bytes :: Word64

  -- -----------------------------------
  -- Cumulative stats about time use
  -- (we use signed values here because due to inaccuracies in timers
  -- the values can occasionally go slightly negative)

    -- | Total CPU time used by the mutator
  , mutator_cpu_ns :: RtsTime
    -- | Total elapsed time used by the mutator
  , mutator_elapsed_ns :: RtsTime
    -- | Total CPU time used by the GC
  , gc_cpu_ns :: RtsTime
    -- | Total elapsed time used by the GC
  , gc_elapsed_ns :: RtsTime
    -- | Total CPU time (at the previous GC)
  , cpu_ns :: RtsTime
    -- | Total elapsed time (at the previous GC)
  , elapsed_ns :: RtsTime

    -- | Details about the most recent GC
  , gc :: GCDetails
  } deriving ( Read -- ^ @since 4.10.0.0
             , Show -- ^ @since 4.10.0.0
             )

--
-- | Statistics about a single GC.  This is a mirror of the C @struct
--   GCDetails@ in @RtsAPI.h@, with the field prefixed with @gc_@ to
--   avoid collisions with 'RTSStats'.
--
data GCDetails = GCDetails {
    -- | The generation number of this GC
    gcdetails_gen :: Word32
    -- | Number of threads used in this GC
  , gcdetails_threads :: Word32
    -- | Number of bytes allocated since the previous GC
  , gcdetails_allocated_bytes :: Word64
    -- | Total amount of live data in the heap (incliudes large + compact data)
  , gcdetails_live_bytes :: Word64
    -- | Total amount of live data in large objects
  , gcdetails_large_objects_bytes :: Word64
    -- | Total amount of live data in compact regions
  , gcdetails_compact_bytes :: Word64
    -- | Total amount of slop (wasted memory)
  , gcdetails_slop_bytes :: Word64
    -- | Total amount of memory in use by the RTS
  , gcdetails_mem_in_use_bytes :: Word64
    -- | Total amount of data copied during this GC
  , gcdetails_copied_bytes :: Word64
    -- | In parallel GC, the max amount of data copied by any one thread.
    -- Deprecated.
  , gcdetails_par_max_copied_bytes :: Word64
    -- | In parallel GC, the amount of balanced data copied by all threads
  , gcdetails_par_balanced_copied_bytes :: Word64
    -- | The time elapsed during synchronisation before GC
  , gcdetails_sync_elapsed_ns :: RtsTime
    -- | The CPU time used during GC itself
  , gcdetails_cpu_ns :: RtsTime
    -- | The time elapsed during GC itself
  , gcdetails_elapsed_ns :: RtsTime
  } deriving ( Read -- ^ @since 4.10.0.0
             , Show -- ^ @since 4.10.0.0
             )

-- | Time values from the RTS, using a fixed resolution of nanoseconds.
type RtsTime = Int64

-- | Get current runtime system statistics.
--
-- @since 4.10.0.0
--
getRTSStats :: IO RTSStats
getRTSStats = do
  statsEnabled <- getRTSStatsEnabled
  unless statsEnabled .  ioError $ IOError
    Nothing
    UnsupportedOperation
    ""
    "GHC.Stats.getRTSStats: GC stats not enabled. Use `+RTS -T -RTS' to enable them."
    Nothing
    Nothing
  allocaBytes ((248)) $ \p -> do
{-# LINE 162 "GHC/Stats.hsc" #-}
    getRTSStats_ p
    gcs <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) p
{-# LINE 164 "GHC/Stats.hsc" #-}
    major_gcs <- ((\hsc_ptr -> peekByteOff hsc_ptr 4)) p
{-# LINE 165 "GHC/Stats.hsc" #-}
    allocated_bytes <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) p
{-# LINE 166 "GHC/Stats.hsc" #-}
    max_live_bytes <- ((\hsc_ptr -> peekByteOff hsc_ptr 16)) p
{-# LINE 167 "GHC/Stats.hsc" #-}
    max_large_objects_bytes <- ((\hsc_ptr -> peekByteOff hsc_ptr 24)) p
{-# LINE 168 "GHC/Stats.hsc" #-}
    max_compact_bytes <- ((\hsc_ptr -> peekByteOff hsc_ptr 32)) p
{-# LINE 169 "GHC/Stats.hsc" #-}
    max_slop_bytes <- ((\hsc_ptr -> peekByteOff hsc_ptr 40)) p
{-# LINE 170 "GHC/Stats.hsc" #-}
    max_mem_in_use_bytes <- ((\hsc_ptr -> peekByteOff hsc_ptr 48)) p
{-# LINE 171 "GHC/Stats.hsc" #-}
    cumulative_live_bytes <- ((\hsc_ptr -> peekByteOff hsc_ptr 56)) p
{-# LINE 172 "GHC/Stats.hsc" #-}
    copied_bytes <- ((\hsc_ptr -> peekByteOff hsc_ptr 64)) p
{-# LINE 173 "GHC/Stats.hsc" #-}
    par_copied_bytes <- ((\hsc_ptr -> peekByteOff hsc_ptr 72)) p
{-# LINE 174 "GHC/Stats.hsc" #-}
    cumulative_par_max_copied_bytes <-
      ((\hsc_ptr -> peekByteOff hsc_ptr 80)) p
{-# LINE 176 "GHC/Stats.hsc" #-}
    cumulative_par_balanced_copied_bytes <-
      ((\hsc_ptr -> peekByteOff hsc_ptr 88)) p
{-# LINE 178 "GHC/Stats.hsc" #-}
    mutator_cpu_ns <- ((\hsc_ptr -> peekByteOff hsc_ptr 96)) p
{-# LINE 179 "GHC/Stats.hsc" #-}
    mutator_elapsed_ns <- ((\hsc_ptr -> peekByteOff hsc_ptr 104)) p
{-# LINE 180 "GHC/Stats.hsc" #-}
    gc_cpu_ns <- ((\hsc_ptr -> peekByteOff hsc_ptr 112)) p
{-# LINE 181 "GHC/Stats.hsc" #-}
    gc_elapsed_ns <- ((\hsc_ptr -> peekByteOff hsc_ptr 120)) p
{-# LINE 182 "GHC/Stats.hsc" #-}
    cpu_ns <- ((\hsc_ptr -> peekByteOff hsc_ptr 128)) p
{-# LINE 183 "GHC/Stats.hsc" #-}
    elapsed_ns <- ((\hsc_ptr -> peekByteOff hsc_ptr 136)) p
{-# LINE 184 "GHC/Stats.hsc" #-}
    let pgc = ((\hsc_ptr -> hsc_ptr `plusPtr` 144)) p
{-# LINE 185 "GHC/Stats.hsc" #-}
    gc <- do
      gcdetails_gen <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) pgc
{-# LINE 187 "GHC/Stats.hsc" #-}
      gcdetails_threads <- ((\hsc_ptr -> peekByteOff hsc_ptr 4)) pgc
{-# LINE 188 "GHC/Stats.hsc" #-}
      gcdetails_allocated_bytes <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) pgc
{-# LINE 189 "GHC/Stats.hsc" #-}
      gcdetails_live_bytes <- ((\hsc_ptr -> peekByteOff hsc_ptr 16)) pgc
{-# LINE 190 "GHC/Stats.hsc" #-}
      gcdetails_large_objects_bytes <-
        ((\hsc_ptr -> peekByteOff hsc_ptr 24)) pgc
{-# LINE 192 "GHC/Stats.hsc" #-}
      gcdetails_compact_bytes <- ((\hsc_ptr -> peekByteOff hsc_ptr 32)) pgc
{-# LINE 193 "GHC/Stats.hsc" #-}
      gcdetails_slop_bytes <- ((\hsc_ptr -> peekByteOff hsc_ptr 40)) pgc
{-# LINE 194 "GHC/Stats.hsc" #-}
      gcdetails_mem_in_use_bytes <- ((\hsc_ptr -> peekByteOff hsc_ptr 48)) pgc
{-# LINE 195 "GHC/Stats.hsc" #-}
      gcdetails_copied_bytes <- ((\hsc_ptr -> peekByteOff hsc_ptr 56)) pgc
{-# LINE 196 "GHC/Stats.hsc" #-}
      gcdetails_par_max_copied_bytes <-
        ((\hsc_ptr -> peekByteOff hsc_ptr 64)) pgc
{-# LINE 198 "GHC/Stats.hsc" #-}
      gcdetails_par_balanced_copied_bytes <-
        ((\hsc_ptr -> peekByteOff hsc_ptr 72)) pgc
{-# LINE 200 "GHC/Stats.hsc" #-}
      gcdetails_sync_elapsed_ns <- ((\hsc_ptr -> peekByteOff hsc_ptr 80)) pgc
{-# LINE 201 "GHC/Stats.hsc" #-}
      gcdetails_cpu_ns <- ((\hsc_ptr -> peekByteOff hsc_ptr 88)) pgc
{-# LINE 202 "GHC/Stats.hsc" #-}
      gcdetails_elapsed_ns <- ((\hsc_ptr -> peekByteOff hsc_ptr 96)) pgc
{-# LINE 203 "GHC/Stats.hsc" #-}
      return GCDetails{..}
    return RTSStats{..}
