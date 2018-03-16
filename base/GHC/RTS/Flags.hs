{-# LINE 1 "GHC/RTS/Flags.hsc" #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Accessors to GHC RTS flags.
-- Descriptions of flags can be seen in
-- <https://www.haskell.org/ghc/docs/latest/html/users_guide/runtime_control.html GHC User's Guide>,
-- or by running RTS help message using @+RTS --help@.
--
-- @since 4.8.0.0
--
module GHC.RTS.Flags
  ( RtsTime
  , RTSFlags (..)
  , GiveGCStats (..)
  , GCFlags (..)
  , ConcFlags (..)
  , MiscFlags (..)
  , DebugFlags (..)
  , DoCostCentres (..)
  , CCFlags (..)
  , DoHeapProfile (..)
  , ProfFlags (..)
  , DoTrace (..)
  , TraceFlags (..)
  , TickyFlags (..)
  , ParFlags (..)
  , getRTSFlags
  , getGCFlags
  , getConcFlags
  , getMiscFlags
  , getDebugFlags
  , getCCFlags
  , getProfFlags
  , getTraceFlags
  , getTickyFlags
  , getParFlags
  ) where




import Control.Applicative
import Control.Monad

import Foreign
import Foreign.C

import GHC.Base
import GHC.Enum
import GHC.IO
import GHC.Real
import GHC.Show

-- | @'Time'@ is defined as a @'StgWord64'@ in @stg/Types.h@
--
-- @since 4.8.2.0
type RtsTime = Word64

-- | Should we produce a summary of the garbage collector statistics after the
-- program has exited?
--
-- @since 4.8.2.0
data GiveGCStats
    = NoGCStats
    | CollectGCStats
    | OneLineGCStats
    | SummaryGCStats
    | VerboseGCStats
    deriving ( Show -- ^ @since 4.8.0.0
             )

-- | @since 4.8.0.0
instance Enum GiveGCStats where
    fromEnum NoGCStats      = 0
{-# LINE 75 "GHC/RTS/Flags.hsc" #-}
    fromEnum CollectGCStats = 1
{-# LINE 76 "GHC/RTS/Flags.hsc" #-}
    fromEnum OneLineGCStats = 2
{-# LINE 77 "GHC/RTS/Flags.hsc" #-}
    fromEnum SummaryGCStats = 3
{-# LINE 78 "GHC/RTS/Flags.hsc" #-}
    fromEnum VerboseGCStats = 4
{-# LINE 79 "GHC/RTS/Flags.hsc" #-}

    toEnum 0      = NoGCStats
{-# LINE 81 "GHC/RTS/Flags.hsc" #-}
    toEnum 1 = CollectGCStats
{-# LINE 82 "GHC/RTS/Flags.hsc" #-}
    toEnum 2 = OneLineGCStats
{-# LINE 83 "GHC/RTS/Flags.hsc" #-}
    toEnum 3 = SummaryGCStats
{-# LINE 84 "GHC/RTS/Flags.hsc" #-}
    toEnum 4 = VerboseGCStats
{-# LINE 85 "GHC/RTS/Flags.hsc" #-}
    toEnum e = errorWithoutStackTrace ("invalid enum for GiveGCStats: " ++ show e)

-- | Parameters of the garbage collector.
--
-- @since 4.8.0.0
data GCFlags = GCFlags
    { statsFile             :: Maybe FilePath
    , giveStats             :: GiveGCStats
    , maxStkSize            :: Word32
    , initialStkSize        :: Word32
    , stkChunkSize          :: Word32
    , stkChunkBufferSize    :: Word32
    , maxHeapSize           :: Word32
    , minAllocAreaSize      :: Word32
    , largeAllocLim         :: Word32
    , nurseryChunkSize      :: Word32
    , minOldGenSize         :: Word32
    , heapSizeSuggestion    :: Word32
    , heapSizeSuggestionAuto :: Bool
    , oldGenFactor          :: Double
    , pcFreeHeap            :: Double
    , generations           :: Word32
    , squeezeUpdFrames      :: Bool
    , compact               :: Bool -- ^ True <=> "compact all the time"
    , compactThreshold      :: Double
    , sweep                 :: Bool
      -- ^ use "mostly mark-sweep" instead of copying for the oldest generation
    , ringBell              :: Bool
    , idleGCDelayTime       :: RtsTime
    , doIdleGC              :: Bool
    , heapBase              :: Word -- ^ address to ask the OS for memory
    , allocLimitGrace       :: Word
    , numa                  :: Bool
    , numaMask              :: Word
    } deriving ( Show -- ^ @since 4.8.0.0
               )

-- | Parameters concerning context switching
--
-- @since 4.8.0.0
data ConcFlags = ConcFlags
    { ctxtSwitchTime  :: RtsTime
    , ctxtSwitchTicks :: Int
    } deriving ( Show -- ^ @since 4.8.0.0
               )

-- | Miscellaneous parameters
--
-- @since 4.8.0.0
data MiscFlags = MiscFlags
    { tickInterval          :: RtsTime
    , installSignalHandlers :: Bool
    , installSEHHandlers    :: Bool
    , generateCrashDumpFile :: Bool
    , generateStackTrace    :: Bool
    , machineReadable       :: Bool
    , linkerMemBase         :: Word
      -- ^ address to ask the OS for memory for the linker, 0 ==> off
    } deriving ( Show -- ^ @since 4.8.0.0
               )

-- | Flags to control debugging output & extra checking in various
-- subsystems.
--
-- @since 4.8.0.0
data DebugFlags = DebugFlags
    { scheduler   :: Bool -- ^ 's'
    , interpreter :: Bool -- ^ 'i'
    , weak        :: Bool -- ^ 'w'
    , gccafs      :: Bool -- ^ 'G'
    , gc          :: Bool -- ^ 'g'
    , block_alloc :: Bool -- ^ 'b'
    , sanity      :: Bool -- ^ 'S'
    , stable      :: Bool -- ^ 't'
    , prof        :: Bool -- ^ 'p'
    , linker      :: Bool -- ^ 'l' the object linker
    , apply       :: Bool -- ^ 'a'
    , stm         :: Bool -- ^ 'm'
    , squeeze     :: Bool -- ^ 'z' stack squeezing & lazy blackholing
    , hpc         :: Bool -- ^ 'c' coverage
    , sparks      :: Bool -- ^ 'r'
    } deriving ( Show -- ^ @since 4.8.0.0
               )

-- | Should the RTS produce a cost-center summary?
--
-- @since 4.8.2.0
data DoCostCentres
    = CostCentresNone
    | CostCentresSummary
    | CostCentresVerbose
    | CostCentresAll
    | CostCentresJSON
    deriving ( Show -- ^ @since 4.8.0.0
             )

-- | @since 4.8.0.0
instance Enum DoCostCentres where
    fromEnum CostCentresNone    = 0
{-# LINE 184 "GHC/RTS/Flags.hsc" #-}
    fromEnum CostCentresSummary = 1
{-# LINE 185 "GHC/RTS/Flags.hsc" #-}
    fromEnum CostCentresVerbose = 2
{-# LINE 186 "GHC/RTS/Flags.hsc" #-}
    fromEnum CostCentresAll     = 3
{-# LINE 187 "GHC/RTS/Flags.hsc" #-}
    fromEnum CostCentresJSON    = 4
{-# LINE 188 "GHC/RTS/Flags.hsc" #-}

    toEnum 0    = CostCentresNone
{-# LINE 190 "GHC/RTS/Flags.hsc" #-}
    toEnum 1 = CostCentresSummary
{-# LINE 191 "GHC/RTS/Flags.hsc" #-}
    toEnum 2 = CostCentresVerbose
{-# LINE 192 "GHC/RTS/Flags.hsc" #-}
    toEnum 3     = CostCentresAll
{-# LINE 193 "GHC/RTS/Flags.hsc" #-}
    toEnum 4    = CostCentresJSON
{-# LINE 194 "GHC/RTS/Flags.hsc" #-}
    toEnum e = errorWithoutStackTrace ("invalid enum for DoCostCentres: " ++ show e)

-- | Parameters pertaining to the cost-center profiler.
--
-- @since 4.8.0.0
data CCFlags = CCFlags
    { doCostCentres :: DoCostCentres
    , profilerTicks :: Int
    , msecsPerTick  :: Int
    } deriving ( Show -- ^ @since 4.8.0.0
               )

-- | What sort of heap profile are we collecting?
--
-- @since 4.8.2.0
data DoHeapProfile
    = NoHeapProfiling
    | HeapByCCS
    | HeapByMod
    | HeapByDescr
    | HeapByType
    | HeapByRetainer
    | HeapByLDV
    | HeapByClosureType
    deriving ( Show -- ^ @since 4.8.0.0
             )

-- | @since 4.8.0.0
instance Enum DoHeapProfile where
    fromEnum NoHeapProfiling   = 0
{-# LINE 224 "GHC/RTS/Flags.hsc" #-}
    fromEnum HeapByCCS         = 1
{-# LINE 225 "GHC/RTS/Flags.hsc" #-}
    fromEnum HeapByMod         = 2
{-# LINE 226 "GHC/RTS/Flags.hsc" #-}
    fromEnum HeapByDescr       = 4
{-# LINE 227 "GHC/RTS/Flags.hsc" #-}
    fromEnum HeapByType        = 5
{-# LINE 228 "GHC/RTS/Flags.hsc" #-}
    fromEnum HeapByRetainer    = 6
{-# LINE 229 "GHC/RTS/Flags.hsc" #-}
    fromEnum HeapByLDV         = 7
{-# LINE 230 "GHC/RTS/Flags.hsc" #-}
    fromEnum HeapByClosureType = 8
{-# LINE 231 "GHC/RTS/Flags.hsc" #-}

    toEnum 0    = NoHeapProfiling
{-# LINE 233 "GHC/RTS/Flags.hsc" #-}
    toEnum 1          = HeapByCCS
{-# LINE 234 "GHC/RTS/Flags.hsc" #-}
    toEnum 2          = HeapByMod
{-# LINE 235 "GHC/RTS/Flags.hsc" #-}
    toEnum 4        = HeapByDescr
{-# LINE 236 "GHC/RTS/Flags.hsc" #-}
    toEnum 5         = HeapByType
{-# LINE 237 "GHC/RTS/Flags.hsc" #-}
    toEnum 6     = HeapByRetainer
{-# LINE 238 "GHC/RTS/Flags.hsc" #-}
    toEnum 7          = HeapByLDV
{-# LINE 239 "GHC/RTS/Flags.hsc" #-}
    toEnum 8 = HeapByClosureType
{-# LINE 240 "GHC/RTS/Flags.hsc" #-}
    toEnum e = errorWithoutStackTrace ("invalid enum for DoHeapProfile: " ++ show e)

-- | Parameters of the cost-center profiler
--
-- @since 4.8.0.0
data ProfFlags = ProfFlags
    { doHeapProfile            :: DoHeapProfile
    , heapProfileInterval      :: RtsTime -- ^ time between samples
    , heapProfileIntervalTicks :: Word    -- ^ ticks between samples (derived)
    , includeTSOs              :: Bool
    , showCCSOnException       :: Bool
    , maxRetainerSetSize       :: Word
    , ccsLength                :: Word
    , modSelector              :: Maybe String
    , descrSelector            :: Maybe String
    , typeSelector             :: Maybe String
    , ccSelector               :: Maybe String
    , ccsSelector              :: Maybe String
    , retainerSelector         :: Maybe String
    , bioSelector              :: Maybe String
    } deriving ( Show -- ^ @since 4.8.0.0
               )

-- | Is event tracing enabled?
--
-- @since 4.8.2.0
data DoTrace
    = TraceNone      -- ^ no tracing
    | TraceEventLog  -- ^ send tracing events to the event log
    | TraceStderr    -- ^ send tracing events to @stderr@
    deriving ( Show -- ^ @since 4.8.0.0
             )

-- | @since 4.8.0.0
instance Enum DoTrace where
    fromEnum TraceNone     = 0
{-# LINE 276 "GHC/RTS/Flags.hsc" #-}
    fromEnum TraceEventLog = 1
{-# LINE 277 "GHC/RTS/Flags.hsc" #-}
    fromEnum TraceStderr   = 2
{-# LINE 278 "GHC/RTS/Flags.hsc" #-}

    toEnum 0     = TraceNone
{-# LINE 280 "GHC/RTS/Flags.hsc" #-}
    toEnum 1 = TraceEventLog
{-# LINE 281 "GHC/RTS/Flags.hsc" #-}
    toEnum 2   = TraceStderr
{-# LINE 282 "GHC/RTS/Flags.hsc" #-}
    toEnum e = errorWithoutStackTrace ("invalid enum for DoTrace: " ++ show e)

-- | Parameters pertaining to event tracing
--
-- @since 4.8.0.0
data TraceFlags = TraceFlags
    { tracing        :: DoTrace
    , timestamp      :: Bool -- ^ show timestamp in stderr output
    , traceScheduler :: Bool -- ^ trace scheduler events
    , traceGc        :: Bool -- ^ trace GC events
    , sparksSampled  :: Bool -- ^ trace spark events by a sampled method
    , sparksFull     :: Bool -- ^ trace spark events 100% accurately
    , user           :: Bool -- ^ trace user events (emitted from Haskell code)
    } deriving ( Show -- ^ @since 4.8.0.0
               )

-- | Parameters pertaining to ticky-ticky profiler
--
-- @since 4.8.0.0
data TickyFlags = TickyFlags
    { showTickyStats :: Bool
    , tickyFile      :: Maybe FilePath
    } deriving ( Show -- ^ @since 4.8.0.0
               )

-- | Parameters pertaining to parallelism
--
-- @since 4.8.0.0
data ParFlags = ParFlags
    { nCapabilities :: Word32
    , migrate :: Bool
    , maxLocalSparks :: Word32
    , parGcEnabled :: Bool
    , parGcGen :: Word32
    , parGcLoadBalancingEnabled :: Bool
    , parGcLoadBalancingGen :: Word32
    , parGcNoSyncWithIdle :: Word32
    , parGcThreads :: Word32
    , setAffinity :: Bool
    }
    deriving ( Show -- ^ @since 4.8.0.0
             )

-- | Parameters of the runtime system
--
-- @since 4.8.0.0
data RTSFlags = RTSFlags
    { gcFlags         :: GCFlags
    , concurrentFlags :: ConcFlags
    , miscFlags       :: MiscFlags
    , debugFlags      :: DebugFlags
    , costCentreFlags :: CCFlags
    , profilingFlags  :: ProfFlags
    , traceFlags      :: TraceFlags
    , tickyFlags      :: TickyFlags
    , parFlags        :: ParFlags
    } deriving ( Show -- ^ @since 4.8.0.0
               )

foreign import ccall "&RtsFlags" rtsFlagsPtr :: Ptr RTSFlags

getRTSFlags :: IO RTSFlags
getRTSFlags = do
  RTSFlags <$> getGCFlags
           <*> getConcFlags
           <*> getMiscFlags
           <*> getDebugFlags
           <*> getCCFlags
           <*> getProfFlags
           <*> getTraceFlags
           <*> getTickyFlags
           <*> getParFlags

peekFilePath :: Ptr () -> IO (Maybe FilePath)
peekFilePath ptr
  | ptr == nullPtr = return Nothing
  | otherwise      = return (Just "<filepath>")

-- | Read a NUL terminated string. Return Nothing in case of a NULL pointer.
peekCStringOpt :: Ptr CChar -> IO (Maybe String)
peekCStringOpt ptr
  | ptr == nullPtr = return Nothing
  | otherwise      = Just <$> peekCString ptr

getGCFlags :: IO GCFlags
getGCFlags = do
  let ptr = ((\hsc_ptr -> hsc_ptr `plusPtr` 0)) rtsFlagsPtr
{-# LINE 369 "GHC/RTS/Flags.hsc" #-}
  GCFlags <$> (peekFilePath =<< (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr)
{-# LINE 370 "GHC/RTS/Flags.hsc" #-}
          <*> (toEnum . fromIntegral <$>
                ((\hsc_ptr -> peekByteOff hsc_ptr 8) ptr :: IO Word32))
{-# LINE 372 "GHC/RTS/Flags.hsc" #-}
          <*> (\hsc_ptr -> peekByteOff hsc_ptr 12) ptr
{-# LINE 373 "GHC/RTS/Flags.hsc" #-}
          <*> (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr
{-# LINE 374 "GHC/RTS/Flags.hsc" #-}
          <*> (\hsc_ptr -> peekByteOff hsc_ptr 20) ptr
{-# LINE 375 "GHC/RTS/Flags.hsc" #-}
          <*> (\hsc_ptr -> peekByteOff hsc_ptr 24) ptr
{-# LINE 376 "GHC/RTS/Flags.hsc" #-}
          <*> (\hsc_ptr -> peekByteOff hsc_ptr 28) ptr
{-# LINE 377 "GHC/RTS/Flags.hsc" #-}
          <*> (\hsc_ptr -> peekByteOff hsc_ptr 32) ptr
{-# LINE 378 "GHC/RTS/Flags.hsc" #-}
          <*> (\hsc_ptr -> peekByteOff hsc_ptr 36) ptr
{-# LINE 379 "GHC/RTS/Flags.hsc" #-}
          <*> (\hsc_ptr -> peekByteOff hsc_ptr 40) ptr
{-# LINE 380 "GHC/RTS/Flags.hsc" #-}
          <*> (\hsc_ptr -> peekByteOff hsc_ptr 44) ptr
{-# LINE 381 "GHC/RTS/Flags.hsc" #-}
          <*> (\hsc_ptr -> peekByteOff hsc_ptr 48) ptr
{-# LINE 382 "GHC/RTS/Flags.hsc" #-}
          <*> (toBool <$>
                ((\hsc_ptr -> peekByteOff hsc_ptr 52) ptr :: IO CBool))
{-# LINE 384 "GHC/RTS/Flags.hsc" #-}
          <*> (\hsc_ptr -> peekByteOff hsc_ptr 56) ptr
{-# LINE 385 "GHC/RTS/Flags.hsc" #-}
          <*> (\hsc_ptr -> peekByteOff hsc_ptr 64) ptr
{-# LINE 386 "GHC/RTS/Flags.hsc" #-}
          <*> (\hsc_ptr -> peekByteOff hsc_ptr 72) ptr
{-# LINE 387 "GHC/RTS/Flags.hsc" #-}
          <*> (toBool <$>
                ((\hsc_ptr -> peekByteOff hsc_ptr 76) ptr :: IO CBool))
{-# LINE 389 "GHC/RTS/Flags.hsc" #-}
          <*> (toBool <$>
                ((\hsc_ptr -> peekByteOff hsc_ptr 77) ptr :: IO CBool))
{-# LINE 391 "GHC/RTS/Flags.hsc" #-}
          <*> (\hsc_ptr -> peekByteOff hsc_ptr 80) ptr
{-# LINE 392 "GHC/RTS/Flags.hsc" #-}
          <*> (toBool <$>
                ((\hsc_ptr -> peekByteOff hsc_ptr 88) ptr :: IO CBool))
{-# LINE 394 "GHC/RTS/Flags.hsc" #-}
          <*> (toBool <$>
                ((\hsc_ptr -> peekByteOff hsc_ptr 89) ptr :: IO CBool))
{-# LINE 396 "GHC/RTS/Flags.hsc" #-}
          <*> (\hsc_ptr -> peekByteOff hsc_ptr 96) ptr
{-# LINE 397 "GHC/RTS/Flags.hsc" #-}
          <*> (toBool <$>
                ((\hsc_ptr -> peekByteOff hsc_ptr 104) ptr :: IO CBool))
{-# LINE 399 "GHC/RTS/Flags.hsc" #-}
          <*> (\hsc_ptr -> peekByteOff hsc_ptr 120) ptr
{-# LINE 400 "GHC/RTS/Flags.hsc" #-}
          <*> (\hsc_ptr -> peekByteOff hsc_ptr 128) ptr
{-# LINE 401 "GHC/RTS/Flags.hsc" #-}
          <*> (toBool <$>
                ((\hsc_ptr -> peekByteOff hsc_ptr 144) ptr :: IO CBool))
{-# LINE 403 "GHC/RTS/Flags.hsc" #-}
          <*> (\hsc_ptr -> peekByteOff hsc_ptr 152) ptr
{-# LINE 404 "GHC/RTS/Flags.hsc" #-}

getParFlags :: IO ParFlags
getParFlags = do
  let ptr = ((\hsc_ptr -> hsc_ptr `plusPtr` 368)) rtsFlagsPtr
{-# LINE 408 "GHC/RTS/Flags.hsc" #-}
  ParFlags
    <$> (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 410 "GHC/RTS/Flags.hsc" #-}
    <*> (toBool <$>
          ((\hsc_ptr -> peekByteOff hsc_ptr 4) ptr :: IO CBool))
{-# LINE 412 "GHC/RTS/Flags.hsc" #-}
    <*> (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr
{-# LINE 413 "GHC/RTS/Flags.hsc" #-}
    <*> (toBool <$>
          ((\hsc_ptr -> peekByteOff hsc_ptr 12) ptr :: IO CBool))
{-# LINE 415 "GHC/RTS/Flags.hsc" #-}
    <*> (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr
{-# LINE 416 "GHC/RTS/Flags.hsc" #-}
    <*> (toBool <$>
          ((\hsc_ptr -> peekByteOff hsc_ptr 20) ptr :: IO CBool))
{-# LINE 418 "GHC/RTS/Flags.hsc" #-}
    <*> (\hsc_ptr -> peekByteOff hsc_ptr 24) ptr
{-# LINE 419 "GHC/RTS/Flags.hsc" #-}
    <*> (\hsc_ptr -> peekByteOff hsc_ptr 28) ptr
{-# LINE 420 "GHC/RTS/Flags.hsc" #-}
    <*> (\hsc_ptr -> peekByteOff hsc_ptr 32) ptr
{-# LINE 421 "GHC/RTS/Flags.hsc" #-}
    <*> (toBool <$>
          ((\hsc_ptr -> peekByteOff hsc_ptr 36) ptr :: IO CBool))
{-# LINE 423 "GHC/RTS/Flags.hsc" #-}

getConcFlags :: IO ConcFlags
getConcFlags = do
  let ptr = ((\hsc_ptr -> hsc_ptr `plusPtr` 160)) rtsFlagsPtr
{-# LINE 427 "GHC/RTS/Flags.hsc" #-}
  ConcFlags <$> (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 428 "GHC/RTS/Flags.hsc" #-}
            <*> (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr
{-# LINE 429 "GHC/RTS/Flags.hsc" #-}

getMiscFlags :: IO MiscFlags
getMiscFlags = do
  let ptr = ((\hsc_ptr -> hsc_ptr `plusPtr` 176)) rtsFlagsPtr
{-# LINE 433 "GHC/RTS/Flags.hsc" #-}
  MiscFlags <$> (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 434 "GHC/RTS/Flags.hsc" #-}
            <*> (toBool <$>
                  ((\hsc_ptr -> peekByteOff hsc_ptr 8) ptr :: IO CBool))
{-# LINE 436 "GHC/RTS/Flags.hsc" #-}
            <*> (toBool <$>
                  ((\hsc_ptr -> peekByteOff hsc_ptr 9) ptr :: IO CBool))
{-# LINE 438 "GHC/RTS/Flags.hsc" #-}
            <*> (toBool <$>
                  ((\hsc_ptr -> peekByteOff hsc_ptr 10) ptr :: IO CBool))
{-# LINE 440 "GHC/RTS/Flags.hsc" #-}
            <*> (toBool <$>
                  ((\hsc_ptr -> peekByteOff hsc_ptr 11) ptr :: IO CBool))
{-# LINE 442 "GHC/RTS/Flags.hsc" #-}
            <*> (toBool <$>
                  ((\hsc_ptr -> peekByteOff hsc_ptr 12) ptr :: IO CBool))
{-# LINE 444 "GHC/RTS/Flags.hsc" #-}
            <*> (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr
{-# LINE 445 "GHC/RTS/Flags.hsc" #-}

getDebugFlags :: IO DebugFlags
getDebugFlags = do
  let ptr = ((\hsc_ptr -> hsc_ptr `plusPtr` 200)) rtsFlagsPtr
{-# LINE 449 "GHC/RTS/Flags.hsc" #-}
  DebugFlags <$> (toBool <$>
                   ((\hsc_ptr -> peekByteOff hsc_ptr 0) ptr :: IO CBool))
{-# LINE 451 "GHC/RTS/Flags.hsc" #-}
             <*> (toBool <$>
                   ((\hsc_ptr -> peekByteOff hsc_ptr 1) ptr :: IO CBool))
{-# LINE 453 "GHC/RTS/Flags.hsc" #-}
             <*> (toBool <$>
                   ((\hsc_ptr -> peekByteOff hsc_ptr 2) ptr :: IO CBool))
{-# LINE 455 "GHC/RTS/Flags.hsc" #-}
             <*> (toBool <$>
                   ((\hsc_ptr -> peekByteOff hsc_ptr 3) ptr :: IO CBool))
{-# LINE 457 "GHC/RTS/Flags.hsc" #-}
             <*> (toBool <$>
                   ((\hsc_ptr -> peekByteOff hsc_ptr 4) ptr :: IO CBool))
{-# LINE 459 "GHC/RTS/Flags.hsc" #-}
             <*> (toBool <$>
                   ((\hsc_ptr -> peekByteOff hsc_ptr 5) ptr :: IO CBool))
{-# LINE 461 "GHC/RTS/Flags.hsc" #-}
             <*> (toBool <$>
                   ((\hsc_ptr -> peekByteOff hsc_ptr 6) ptr :: IO CBool))
{-# LINE 463 "GHC/RTS/Flags.hsc" #-}
             <*> (toBool <$>
                   ((\hsc_ptr -> peekByteOff hsc_ptr 7) ptr :: IO CBool))
{-# LINE 465 "GHC/RTS/Flags.hsc" #-}
             <*> (toBool <$>
                   ((\hsc_ptr -> peekByteOff hsc_ptr 8) ptr :: IO CBool))
{-# LINE 467 "GHC/RTS/Flags.hsc" #-}
             <*> (toBool <$>
                   ((\hsc_ptr -> peekByteOff hsc_ptr 9) ptr :: IO CBool))
{-# LINE 469 "GHC/RTS/Flags.hsc" #-}
             <*> (toBool <$>
                   ((\hsc_ptr -> peekByteOff hsc_ptr 10) ptr :: IO CBool))
{-# LINE 471 "GHC/RTS/Flags.hsc" #-}
             <*> (toBool <$>
                   ((\hsc_ptr -> peekByteOff hsc_ptr 11) ptr :: IO CBool))
{-# LINE 473 "GHC/RTS/Flags.hsc" #-}
             <*> (toBool <$>
                   ((\hsc_ptr -> peekByteOff hsc_ptr 12) ptr :: IO CBool))
{-# LINE 475 "GHC/RTS/Flags.hsc" #-}
             <*> (toBool <$>
                   ((\hsc_ptr -> peekByteOff hsc_ptr 13) ptr :: IO CBool))
{-# LINE 477 "GHC/RTS/Flags.hsc" #-}
             <*> (toBool <$>
                   ((\hsc_ptr -> peekByteOff hsc_ptr 14) ptr :: IO CBool))
{-# LINE 479 "GHC/RTS/Flags.hsc" #-}

getCCFlags :: IO CCFlags
getCCFlags = do
  let ptr = ((\hsc_ptr -> hsc_ptr `plusPtr` 0)) rtsFlagsPtr
{-# LINE 483 "GHC/RTS/Flags.hsc" #-}
  CCFlags <$> (toEnum . fromIntegral
                <$> ((\hsc_ptr -> peekByteOff hsc_ptr 0) ptr :: IO Word32))
{-# LINE 485 "GHC/RTS/Flags.hsc" #-}
          <*> (\hsc_ptr -> peekByteOff hsc_ptr 4) ptr
{-# LINE 486 "GHC/RTS/Flags.hsc" #-}
          <*> (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr
{-# LINE 487 "GHC/RTS/Flags.hsc" #-}

getProfFlags :: IO ProfFlags
getProfFlags = do
  let ptr = ((\hsc_ptr -> hsc_ptr `plusPtr` 248)) rtsFlagsPtr
{-# LINE 491 "GHC/RTS/Flags.hsc" #-}
  ProfFlags <$> (toEnum <$> (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr)
{-# LINE 492 "GHC/RTS/Flags.hsc" #-}
            <*> (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr
{-# LINE 493 "GHC/RTS/Flags.hsc" #-}
            <*> (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr
{-# LINE 494 "GHC/RTS/Flags.hsc" #-}
            <*> (toBool <$>
                  ((\hsc_ptr -> peekByteOff hsc_ptr 20) ptr :: IO CBool))
{-# LINE 496 "GHC/RTS/Flags.hsc" #-}
            <*> (toBool <$>
                  ((\hsc_ptr -> peekByteOff hsc_ptr 21) ptr :: IO CBool))
{-# LINE 498 "GHC/RTS/Flags.hsc" #-}
            <*> (\hsc_ptr -> peekByteOff hsc_ptr 24) ptr
{-# LINE 499 "GHC/RTS/Flags.hsc" #-}
            <*> (\hsc_ptr -> peekByteOff hsc_ptr 28) ptr
{-# LINE 500 "GHC/RTS/Flags.hsc" #-}
            <*> (peekCStringOpt =<< (\hsc_ptr -> peekByteOff hsc_ptr 32) ptr)
{-# LINE 501 "GHC/RTS/Flags.hsc" #-}
            <*> (peekCStringOpt =<< (\hsc_ptr -> peekByteOff hsc_ptr 40) ptr)
{-# LINE 502 "GHC/RTS/Flags.hsc" #-}
            <*> (peekCStringOpt =<< (\hsc_ptr -> peekByteOff hsc_ptr 48) ptr)
{-# LINE 503 "GHC/RTS/Flags.hsc" #-}
            <*> (peekCStringOpt =<< (\hsc_ptr -> peekByteOff hsc_ptr 56) ptr)
{-# LINE 504 "GHC/RTS/Flags.hsc" #-}
            <*> (peekCStringOpt =<< (\hsc_ptr -> peekByteOff hsc_ptr 64) ptr)
{-# LINE 505 "GHC/RTS/Flags.hsc" #-}
            <*> (peekCStringOpt =<< (\hsc_ptr -> peekByteOff hsc_ptr 72) ptr)
{-# LINE 506 "GHC/RTS/Flags.hsc" #-}
            <*> (peekCStringOpt =<< (\hsc_ptr -> peekByteOff hsc_ptr 80) ptr)
{-# LINE 507 "GHC/RTS/Flags.hsc" #-}

getTraceFlags :: IO TraceFlags
getTraceFlags = do
  let ptr = ((\hsc_ptr -> hsc_ptr `plusPtr` 336)) rtsFlagsPtr
{-# LINE 511 "GHC/RTS/Flags.hsc" #-}
  TraceFlags <$> (toEnum . fromIntegral
                   <$> ((\hsc_ptr -> peekByteOff hsc_ptr 0) ptr :: IO CInt))
{-# LINE 513 "GHC/RTS/Flags.hsc" #-}
             <*> (toBool <$>
                   ((\hsc_ptr -> peekByteOff hsc_ptr 4) ptr :: IO CBool))
{-# LINE 515 "GHC/RTS/Flags.hsc" #-}
             <*> (toBool <$>
                   ((\hsc_ptr -> peekByteOff hsc_ptr 5) ptr :: IO CBool))
{-# LINE 517 "GHC/RTS/Flags.hsc" #-}
             <*> (toBool <$>
                   ((\hsc_ptr -> peekByteOff hsc_ptr 6) ptr :: IO CBool))
{-# LINE 519 "GHC/RTS/Flags.hsc" #-}
             <*> (toBool <$>
                   ((\hsc_ptr -> peekByteOff hsc_ptr 7) ptr :: IO CBool))
{-# LINE 521 "GHC/RTS/Flags.hsc" #-}
             <*> (toBool <$>
                   ((\hsc_ptr -> peekByteOff hsc_ptr 8) ptr :: IO CBool))
{-# LINE 523 "GHC/RTS/Flags.hsc" #-}
             <*> (toBool <$>
                   ((\hsc_ptr -> peekByteOff hsc_ptr 9) ptr :: IO CBool))
{-# LINE 525 "GHC/RTS/Flags.hsc" #-}

getTickyFlags :: IO TickyFlags
getTickyFlags = do
  let ptr = ((\hsc_ptr -> hsc_ptr `plusPtr` 352)) rtsFlagsPtr
{-# LINE 529 "GHC/RTS/Flags.hsc" #-}
  TickyFlags <$> (toBool <$>
                   ((\hsc_ptr -> peekByteOff hsc_ptr 0) ptr :: IO CBool))
{-# LINE 531 "GHC/RTS/Flags.hsc" #-}
             <*> (peekFilePath =<< (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr)
{-# LINE 532 "GHC/RTS/Flags.hsc" #-}
