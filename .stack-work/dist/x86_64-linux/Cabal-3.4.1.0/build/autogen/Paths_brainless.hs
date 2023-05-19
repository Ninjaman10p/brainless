{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_brainless (
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

bindir     = "/home/paelias/Dev/brainless/.stack-work/install/x86_64-linux/175168733d6ea6c86358e725f300f5316af11cf668b8be40d242428bb6db7e14/9.0.2/bin"
libdir     = "/home/paelias/Dev/brainless/.stack-work/install/x86_64-linux/175168733d6ea6c86358e725f300f5316af11cf668b8be40d242428bb6db7e14/9.0.2/lib/x86_64-linux-ghc-9.0.2/brainless-0.1.0.0-5tGk4oLYrF7Ce5zGcOpTyi"
dynlibdir  = "/home/paelias/Dev/brainless/.stack-work/install/x86_64-linux/175168733d6ea6c86358e725f300f5316af11cf668b8be40d242428bb6db7e14/9.0.2/lib/x86_64-linux-ghc-9.0.2"
datadir    = "/home/paelias/Dev/brainless/.stack-work/install/x86_64-linux/175168733d6ea6c86358e725f300f5316af11cf668b8be40d242428bb6db7e14/9.0.2/share/x86_64-linux-ghc-9.0.2/brainless-0.1.0.0"
libexecdir = "/home/paelias/Dev/brainless/.stack-work/install/x86_64-linux/175168733d6ea6c86358e725f300f5316af11cf668b8be40d242428bb6db7e14/9.0.2/libexec/x86_64-linux-ghc-9.0.2/brainless-0.1.0.0"
sysconfdir = "/home/paelias/Dev/brainless/.stack-work/install/x86_64-linux/175168733d6ea6c86358e725f300f5316af11cf668b8be40d242428bb6db7e14/9.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "brainless_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "brainless_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "brainless_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "brainless_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "brainless_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "brainless_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
