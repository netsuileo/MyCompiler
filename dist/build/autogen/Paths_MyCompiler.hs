module Paths_MyCompiler (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/leo/.cabal/bin"
libdir     = "/home/leo/.cabal/lib/x86_64-linux-ghc-7.8.3/MyCompiler-0.1"
datadir    = "/home/leo/.cabal/share/x86_64-linux-ghc-7.8.3/MyCompiler-0.1"
libexecdir = "/home/leo/.cabal/libexec"
sysconfdir = "/home/leo/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "MyCompiler_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "MyCompiler_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "MyCompiler_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "MyCompiler_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "MyCompiler_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
