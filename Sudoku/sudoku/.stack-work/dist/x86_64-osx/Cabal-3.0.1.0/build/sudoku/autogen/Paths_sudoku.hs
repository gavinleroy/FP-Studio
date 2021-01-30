{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_sudoku (
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

bindir     = "/Users/gavingray/codefiles/classes/CS6963/SudokuEnv/sudoku/.stack-work/install/x86_64-osx/faf55e386712cfdce85d6ebcd4b614138e8aa116f2486cbafb60a6aef812e7af/8.8.4/bin"
libdir     = "/Users/gavingray/codefiles/classes/CS6963/SudokuEnv/sudoku/.stack-work/install/x86_64-osx/faf55e386712cfdce85d6ebcd4b614138e8aa116f2486cbafb60a6aef812e7af/8.8.4/lib/x86_64-osx-ghc-8.8.4/sudoku-0.1.0.0-7VQYgnqu3yy5U92Be2erta-sudoku"
dynlibdir  = "/Users/gavingray/codefiles/classes/CS6963/SudokuEnv/sudoku/.stack-work/install/x86_64-osx/faf55e386712cfdce85d6ebcd4b614138e8aa116f2486cbafb60a6aef812e7af/8.8.4/lib/x86_64-osx-ghc-8.8.4"
datadir    = "/Users/gavingray/codefiles/classes/CS6963/SudokuEnv/sudoku/.stack-work/install/x86_64-osx/faf55e386712cfdce85d6ebcd4b614138e8aa116f2486cbafb60a6aef812e7af/8.8.4/share/x86_64-osx-ghc-8.8.4/sudoku-0.1.0.0"
libexecdir = "/Users/gavingray/codefiles/classes/CS6963/SudokuEnv/sudoku/.stack-work/install/x86_64-osx/faf55e386712cfdce85d6ebcd4b614138e8aa116f2486cbafb60a6aef812e7af/8.8.4/libexec/x86_64-osx-ghc-8.8.4/sudoku-0.1.0.0"
sysconfdir = "/Users/gavingray/codefiles/classes/CS6963/SudokuEnv/sudoku/.stack-work/install/x86_64-osx/faf55e386712cfdce85d6ebcd4b614138e8aa116f2486cbafb60a6aef812e7af/8.8.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "sudoku_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "sudoku_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "sudoku_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "sudoku_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "sudoku_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "sudoku_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
