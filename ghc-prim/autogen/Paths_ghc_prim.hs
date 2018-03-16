{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_ghc_prim (
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
version = Version [0,5,2,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/tmp/asterius/.boot/bin"
libdir     = "/tmp/asterius/.boot/lib/x86_64-linux-ghc-8.5.20180310/ghc-prim-0.5.2.0-Bfo9y0qb0emG5VRfx5d4mv"
dynlibdir  = "/tmp/asterius/.boot/lib/x86_64-linux-ghc-8.5.20180310"
datadir    = "/tmp/asterius/.boot/share/x86_64-linux-ghc-8.5.20180310/ghc-prim-0.5.2.0"
libexecdir = "/tmp/asterius/.boot/libexec/x86_64-linux-ghc-8.5.20180310/ghc-prim-0.5.2.0"
sysconfdir = "/tmp/asterius/.boot/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "ghc_prim_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "ghc_prim_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "ghc_prim_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "ghc_prim_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ghc_prim_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "ghc_prim_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
