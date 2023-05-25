{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_social_network_simulation (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/Users/sayedsohail/.cabal/bin"
libdir     = "/Users/sayedsohail/.cabal/lib/aarch64-osx-ghc-9.4.4/social-network-simulation-0.1.0.0-inplace"
dynlibdir  = "/Users/sayedsohail/.cabal/lib/aarch64-osx-ghc-9.4.4"
datadir    = "/Users/sayedsohail/.cabal/share/aarch64-osx-ghc-9.4.4/social-network-simulation-0.1.0.0"
libexecdir = "/Users/sayedsohail/.cabal/libexec/aarch64-osx-ghc-9.4.4/social-network-simulation-0.1.0.0"
sysconfdir = "/Users/sayedsohail/.cabal/etc"

getBinDir     = catchIO (getEnv "social_network_simulation_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "social_network_simulation_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "social_network_simulation_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "social_network_simulation_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "social_network_simulation_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "social_network_simulation_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
