{-# LINE 1 "System/Environment/Blank.hsc" #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE CApiFFI #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  System.Environment.Blank
-- Copyright   :  (c) Habib Alamin 2017
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- A setEnv implementation that allows blank environment variables. Mimics
-- the `System.Posix.Env` module from the @unix@ package, but with support
-- for Windows too.
--
-- The matrix of platforms that:
--
--   * support putenv("FOO") to unset environment variables,
--   * support putenv("FOO=") to unset environment variables or set them
--     to blank values,
--   * support unsetenv to unset environment variables,
--   * support setenv to set environment variables,
--   * etc.
--
-- is very complicated. I think AIX is screwed, but we don't support it.
-- The whole situation with setenv(3), unsetenv(3), and putenv(3) is not
-- good. Even mingw32 adds its own crap to the pile, but luckily, we can
-- just use Windows' native environment functions to sidestep the issue.
--
-- #12494
--
-----------------------------------------------------------------------------

module System.Environment.Blank
    (
      module System.Environment,
      getEnv,
      getEnvDefault,
      setEnv,
      unsetEnv,
  ) where

import Foreign.C

{-# LINE 52 "System/Environment/Blank.hsc" #-}
import System.Posix.Internals

{-# LINE 54 "System/Environment/Blank.hsc" #-}
import GHC.IO.Exception
import System.IO.Error
import Control.Exception.Base
import Data.Maybe

import System.Environment
    (
      getArgs,
      getProgName,
      getExecutablePath,
      withArgs,
      withProgName,
      getEnvironment
  )

{-# LINE 69 "System/Environment/Blank.hsc" #-}
import qualified System.Environment as Environment

{-# LINE 71 "System/Environment/Blank.hsc" #-}

-- TODO: include windows_cconv.h when it's merged, instead of duplicating
-- this C macro block.

{-# LINE 83 "System/Environment/Blank.hsc" #-}



throwInvalidArgument :: String -> IO a
throwInvalidArgument from =
  throwIO (mkIOError InvalidArgument from Nothing Nothing)

-- | `System.Environment.lookupEnv`.
getEnv :: String -> IO (Maybe String)

{-# LINE 95 "System/Environment/Blank.hsc" #-}
getEnv = Environment.lookupEnv

{-# LINE 97 "System/Environment/Blank.hsc" #-}

-- | Get an environment value or a default value.
getEnvDefault ::
  String    {- ^ variable name                    -} ->
  String    {- ^ fallback value                   -} ->
  IO String {- ^ variable value or fallback value -}
getEnvDefault name fallback = fromMaybe fallback <$> getEnv name

-- | Like `System.Environment.setEnv`, but allows blank environment values
-- and mimics the function signature of `System.Posix.Env.setEnv` from the
-- @unix@ package.
setEnv ::
  String {- ^ variable name  -} ->
  String {- ^ variable value -} ->
  Bool   {- ^ overwrite      -} ->
  IO ()
setEnv key_ value_ overwrite
  | null key       = throwInvalidArgument "setEnv"
  | '=' `elem` key = throwInvalidArgument "setEnv"
  | otherwise      =
    if overwrite
    then setEnv_ key value
    else do
      env_var <- getEnv key
      case env_var of
          Just _  -> return ()
          Nothing -> setEnv_ key value
  where
    key   = takeWhile (/= '\NUL') key_
    value = takeWhile (/= '\NUL') value_

setEnv_ :: String -> String -> IO ()

{-# LINE 137 "System/Environment/Blank.hsc" #-}
setEnv_ key value =
  withFilePath key $ \ keyP ->
    withFilePath value $ \ valueP ->
      throwErrnoIfMinus1_ "setenv" $
        c_setenv keyP valueP (fromIntegral (fromEnum True))

foreign import ccall unsafe "setenv"
   c_setenv :: CString -> CString -> CInt -> IO CInt

{-# LINE 146 "System/Environment/Blank.hsc" #-}

-- | Like `System.Environment.unsetEnv`, but allows for the removal of
-- blank environment variables.
unsetEnv :: String -> IO ()

{-# LINE 166 "System/Environment/Blank.hsc" #-}

{-# LINE 167 "System/Environment/Blank.hsc" #-}
unsetEnv name = withFilePath name $ \ s ->
  throwErrnoIfMinus1_ "unsetenv" (c_unsetenv s)

-- POSIX.1-2001 compliant unsetenv(3)
foreign import capi unsafe "HsBase.h unsetenv"
   c_unsetenv :: CString -> IO CInt

{-# LINE 180 "System/Environment/Blank.hsc" #-}

{-# LINE 197 "System/Environment/Blank.hsc" #-}
