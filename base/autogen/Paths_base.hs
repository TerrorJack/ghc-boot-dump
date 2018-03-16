{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_base (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [4,11,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/tmp/asterius/.boot/bin"
libdir     = "/tmp/asterius/.boot/lib/x86_64-linux-ghc-8.5.20180310/base-4.11.0.0-1MEl4SemCjh5Wpggve52Ij"
dynlibdir  = "/tmp/asterius/.boot/lib/x86_64-linux-ghc-8.5.20180310"
datadir    = "/tmp/asterius/.boot/share/x86_64-linux-ghc-8.5.20180310/base-4.11.0.0"
libexecdir = "/tmp/asterius/.boot/libexec/x86_64-linux-ghc-8.5.20180310/base-4.11.0.0"
sysconfdir = "/tmp/asterius/.boot/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "base_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "base_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "base_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "base_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "base_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "base_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
