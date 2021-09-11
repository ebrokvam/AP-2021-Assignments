{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_warmup (
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
version = Version [0,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\ebrok\\Documents\\Msc in Computer Science\\Year 2\\Advanced Programming\\Assignment 1\\code\\part1\\.stack-work\\install\\3552fc11\\bin"
libdir     = "C:\\Users\\ebrok\\Documents\\Msc in Computer Science\\Year 2\\Advanced Programming\\Assignment 1\\code\\part1\\.stack-work\\install\\3552fc11\\lib\\x86_64-windows-ghc-8.10.6\\warmup-0.0.0-FGNpr20rj0E4cd13XF0tZB"
dynlibdir  = "C:\\Users\\ebrok\\Documents\\Msc in Computer Science\\Year 2\\Advanced Programming\\Assignment 1\\code\\part1\\.stack-work\\install\\3552fc11\\lib\\x86_64-windows-ghc-8.10.6"
datadir    = "C:\\Users\\ebrok\\Documents\\Msc in Computer Science\\Year 2\\Advanced Programming\\Assignment 1\\code\\part1\\.stack-work\\install\\3552fc11\\share\\x86_64-windows-ghc-8.10.6\\warmup-0.0.0"
libexecdir = "C:\\Users\\ebrok\\Documents\\Msc in Computer Science\\Year 2\\Advanced Programming\\Assignment 1\\code\\part1\\.stack-work\\install\\3552fc11\\libexec\\x86_64-windows-ghc-8.10.6\\warmup-0.0.0"
sysconfdir = "C:\\Users\\ebrok\\Documents\\Msc in Computer Science\\Year 2\\Advanced Programming\\Assignment 1\\code\\part1\\.stack-work\\install\\3552fc11\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "warmup_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "warmup_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "warmup_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "warmup_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "warmup_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "warmup_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
