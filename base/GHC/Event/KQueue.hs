{-# LINE 1 "GHC/Event/KQueue.hsc" #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CApiFFI
           , GeneralizedNewtypeDeriving
           , NoImplicitPrelude
           , RecordWildCards
           , BangPatterns
  #-}

module GHC.Event.KQueue
    (
      new
    , available
    ) where

import qualified GHC.Event.Internal as E



{-# LINE 19 "GHC/Event/KQueue.hsc" #-}
import GHC.Base

new :: IO E.Backend
new = errorWithoutStackTrace "KQueue back end not implemented for this platform"

available :: Bool
available = False
{-# INLINE available #-}

{-# LINE 309 "GHC/Event/KQueue.hsc" #-}
