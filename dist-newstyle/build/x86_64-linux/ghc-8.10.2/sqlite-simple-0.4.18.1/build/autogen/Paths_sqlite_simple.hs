{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_sqlite_simple (
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
version = Version [0,4,18,1] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/hihihaha/.cabal/bin"
libdir     = "/home/hihihaha/.cabal/lib/x86_64-linux-ghc-8.10.2/sqlite-simple-0.4.18.1-inplace"
dynlibdir  = "/home/hihihaha/.cabal/lib/x86_64-linux-ghc-8.10.2"
datadir    = "/home/hihihaha/.cabal/share/x86_64-linux-ghc-8.10.2/sqlite-simple-0.4.18.1"
libexecdir = "/home/hihihaha/.cabal/libexec/x86_64-linux-ghc-8.10.2/sqlite-simple-0.4.18.1"
sysconfdir = "/home/hihihaha/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "sqlite_simple_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "sqlite_simple_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "sqlite_simple_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "sqlite_simple_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "sqlite_simple_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "sqlite_simple_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
