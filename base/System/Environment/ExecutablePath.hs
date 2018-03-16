{-# LINE 1 "System/Environment/ExecutablePath.hsc" #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  System.Environment.ExecutablePath
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Function to retrieve the absolute filepath of the current executable.
--
-- @since 4.6.0.0
-----------------------------------------------------------------------------

module System.Environment.ExecutablePath ( getExecutablePath ) where

-- The imports are purposely kept completely disjoint to prevent edits
-- to one OS implementation from breaking another.


{-# LINE 32 "System/Environment/ExecutablePath.hsc" #-}
import Foreign.C
import Foreign.Marshal.Array
import System.Posix.Internals

{-# LINE 50 "System/Environment/ExecutablePath.hsc" #-}

-- The exported function is defined outside any if-guard to make sure
-- every OS implements it with the same type.

-- | Returns the absolute pathname of the current executable.
--
-- Note that for scripts and interactive sessions, this is the path to
-- the interpreter (e.g. ghci.)
--
-- Since base 4.11.0.0, 'getExecutablePath' resolves symlinks on Windows.
-- If an executable is launched through a symlink, 'getExecutablePath'
-- returns the absolute path of the original executable.
--
-- @since 4.6.0.0
getExecutablePath :: IO FilePath

--------------------------------------------------------------------------------
-- Mac OS X


{-# LINE 115 "System/Environment/ExecutablePath.hsc" #-}

foreign import ccall unsafe "readlink"
    c_readlink :: CString -> CString -> CSize -> IO CInt

-- | Reads the @FilePath@ pointed to by the symbolic link and returns
-- it.
--
-- See readlink(2)
readSymbolicLink :: FilePath -> IO FilePath
readSymbolicLink file =
    allocaArray0 4096 $ \buf -> do
        withFilePath file $ \s -> do
            len <- throwErrnoPathIfMinus1 "readSymbolicLink" file $
                   c_readlink s buf 4096
            peekFilePathLen (buf,fromIntegral len)

getExecutablePath = readSymbolicLink $ "/proc/self/exe"

--------------------------------------------------------------------------------
-- Windows


{-# LINE 251 "System/Environment/ExecutablePath.hsc" #-}
