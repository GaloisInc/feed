module Paths_feed (
	version,
	getBinDir, getLibDir, getDataDir, getLibexecDir,
	getDataFileName
	) where

import Data.Version

version :: Version
version = Version {versionBranch = [0,1,0], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/sof/lib/packages/bin"
libdir     = "/home/sof/lib/packages/lib/feed-0.1.0/ghc-6.8.1"
datadir    = "/home/sof/lib/packages/share/feed-0.1.0"
libexecdir = "/home/sof/lib/packages/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = return bindir
getLibDir = return libdir
getDataDir = return datadir
getLibexecDir = return libexecdir

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = return (datadir ++ "/" ++ name)
