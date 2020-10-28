{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_ch07 (
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

bindir     = "/home/leolanger/.cabal/bin"
libdir     = "/home/leolanger/.cabal/lib/x86_64-linux-ghc-8.10.2/ch07-0.1.0.0-inplace-ch07-exe"
dynlibdir  = "/home/leolanger/.cabal/lib/x86_64-linux-ghc-8.10.2"
datadir    = "/home/leolanger/.cabal/share/x86_64-linux-ghc-8.10.2/ch07-0.1.0.0"
libexecdir = "/home/leolanger/.cabal/libexec/x86_64-linux-ghc-8.10.2/ch07-0.1.0.0"
sysconfdir = "/home/leolanger/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "ch07_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "ch07_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "ch07_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "ch07_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ch07_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "ch07_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
