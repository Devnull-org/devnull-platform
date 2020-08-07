{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_backend (
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
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/v0d1ch/code/devnull/backend/.stack-work/install/x86_64-linux/d64a7ce59f189546be06036f6a73ee2c16433b522d1a93612b3d4a34f8263aef/8.8.3/bin"
libdir     = "/home/v0d1ch/code/devnull/backend/.stack-work/install/x86_64-linux/d64a7ce59f189546be06036f6a73ee2c16433b522d1a93612b3d4a34f8263aef/8.8.3/lib/x86_64-linux-ghc-8.8.3/backend-0.1.0.0-CMbtXNaOHULAZyOh7iij55-backend-exe"
dynlibdir  = "/home/v0d1ch/code/devnull/backend/.stack-work/install/x86_64-linux/d64a7ce59f189546be06036f6a73ee2c16433b522d1a93612b3d4a34f8263aef/8.8.3/lib/x86_64-linux-ghc-8.8.3"
datadir    = "/home/v0d1ch/code/devnull/backend/.stack-work/install/x86_64-linux/d64a7ce59f189546be06036f6a73ee2c16433b522d1a93612b3d4a34f8263aef/8.8.3/share/x86_64-linux-ghc-8.8.3/backend-0.1.0.0"
libexecdir = "/home/v0d1ch/code/devnull/backend/.stack-work/install/x86_64-linux/d64a7ce59f189546be06036f6a73ee2c16433b522d1a93612b3d4a34f8263aef/8.8.3/libexec/x86_64-linux-ghc-8.8.3/backend-0.1.0.0"
sysconfdir = "/home/v0d1ch/code/devnull/backend/.stack-work/install/x86_64-linux/d64a7ce59f189546be06036f6a73ee2c16433b522d1a93612b3d4a34f8263aef/8.8.3/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "backend_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "backend_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "backend_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "backend_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "backend_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "backend_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
