{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_weight_tracker (
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

bindir     = "/Users/matt/dev/haskell/weight-tracker/.stack-work/install/x86_64-osx/a080f9a97fb4bc58f7a7038bb621f531019842fac74fb4df89043c13a3b66da6/8.6.5/bin"
libdir     = "/Users/matt/dev/haskell/weight-tracker/.stack-work/install/x86_64-osx/a080f9a97fb4bc58f7a7038bb621f531019842fac74fb4df89043c13a3b66da6/8.6.5/lib/x86_64-osx-ghc-8.6.5/weight-tracker-0.1.0.0-4krvW0Be54VLPrz0u3yPz2-weight-tracker"
dynlibdir  = "/Users/matt/dev/haskell/weight-tracker/.stack-work/install/x86_64-osx/a080f9a97fb4bc58f7a7038bb621f531019842fac74fb4df89043c13a3b66da6/8.6.5/lib/x86_64-osx-ghc-8.6.5"
datadir    = "/Users/matt/dev/haskell/weight-tracker/.stack-work/install/x86_64-osx/a080f9a97fb4bc58f7a7038bb621f531019842fac74fb4df89043c13a3b66da6/8.6.5/share/x86_64-osx-ghc-8.6.5/weight-tracker-0.1.0.0"
libexecdir = "/Users/matt/dev/haskell/weight-tracker/.stack-work/install/x86_64-osx/a080f9a97fb4bc58f7a7038bb621f531019842fac74fb4df89043c13a3b66da6/8.6.5/libexec/x86_64-osx-ghc-8.6.5/weight-tracker-0.1.0.0"
sysconfdir = "/Users/matt/dev/haskell/weight-tracker/.stack-work/install/x86_64-osx/a080f9a97fb4bc58f7a7038bb621f531019842fac74fb4df89043c13a3b66da6/8.6.5/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "weight_tracker_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "weight_tracker_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "weight_tracker_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "weight_tracker_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "weight_tracker_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "weight_tracker_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
