{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_funcional_uno (
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
bindir     = "/home/servasquezlizana/Documentos/Paradigmas/Funcional/funcional-uno/.stack-work/install/x86_64-linux/cb3b5e10edcf0bd9f880f94b7e31655b619a5fa8ed93967f57f982ba70057fd7/9.6.6/bin"
libdir     = "/home/servasquezlizana/Documentos/Paradigmas/Funcional/funcional-uno/.stack-work/install/x86_64-linux/cb3b5e10edcf0bd9f880f94b7e31655b619a5fa8ed93967f57f982ba70057fd7/9.6.6/lib/x86_64-linux-ghc-9.6.6/funcional-uno-0.1.0.0-CGYfa71Qar93P9hI7EtbXr-funcional-uno-test"
dynlibdir  = "/home/servasquezlizana/Documentos/Paradigmas/Funcional/funcional-uno/.stack-work/install/x86_64-linux/cb3b5e10edcf0bd9f880f94b7e31655b619a5fa8ed93967f57f982ba70057fd7/9.6.6/lib/x86_64-linux-ghc-9.6.6"
datadir    = "/home/servasquezlizana/Documentos/Paradigmas/Funcional/funcional-uno/.stack-work/install/x86_64-linux/cb3b5e10edcf0bd9f880f94b7e31655b619a5fa8ed93967f57f982ba70057fd7/9.6.6/share/x86_64-linux-ghc-9.6.6/funcional-uno-0.1.0.0"
libexecdir = "/home/servasquezlizana/Documentos/Paradigmas/Funcional/funcional-uno/.stack-work/install/x86_64-linux/cb3b5e10edcf0bd9f880f94b7e31655b619a5fa8ed93967f57f982ba70057fd7/9.6.6/libexec/x86_64-linux-ghc-9.6.6/funcional-uno-0.1.0.0"
sysconfdir = "/home/servasquezlizana/Documentos/Paradigmas/Funcional/funcional-uno/.stack-work/install/x86_64-linux/cb3b5e10edcf0bd9f880f94b7e31655b619a5fa8ed93967f57f982ba70057fd7/9.6.6/etc"

getBinDir     = catchIO (getEnv "funcional_uno_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "funcional_uno_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "funcional_uno_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "funcional_uno_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "funcional_uno_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "funcional_uno_sysconfdir") (\_ -> return sysconfdir)



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
