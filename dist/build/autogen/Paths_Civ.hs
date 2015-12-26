module Paths_Civ (
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
version = Version {versionBranch = [0,0,1], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/root/.cabal/bin"
libdir     = "/root/.cabal/lib/i386-linux-ghc-7.8.4/Civ-0.0.1"
datadir    = "/root/.cabal/share/i386-linux-ghc-7.8.4/Civ-0.0.1"
libexecdir = "/root/.cabal/libexec"
sysconfdir = "/root/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Civ_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Civ_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Civ_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Civ_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Civ_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
