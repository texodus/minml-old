module Paths_forml (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,4], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/Users/slink/Library/Haskell/ghc-7.4.2/lib/forml-0.4/bin"
libdir     = "/Users/slink/Library/Haskell/ghc-7.4.2/lib/forml-0.4/lib"
datadir    = "/Users/slink/Library/Haskell/ghc-7.4.2/lib/forml-0.4/share"
libexecdir = "/Users/slink/Library/Haskell/ghc-7.4.2/lib/forml-0.4/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "forml_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "forml_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "forml_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "forml_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
