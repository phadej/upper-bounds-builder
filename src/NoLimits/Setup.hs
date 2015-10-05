{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
module NoLimits.Setup where

import Path
import System.Directory
import System.Process

import NoLimits.Types

mkdirp :: Path Abs Dir -> IO ()
mkdirp = createDirectoryIfMissing True . toFilePath

cmdSetup :: SetupOpts -> IO ()
cmdSetup SetupOpts {..} = do
  mkdirp $ soPath </> $(mkRelDir "cache")
  mkdirp $ soPath </> $(mkRelDir "log")
  mkdirp $ soPath </> $(mkRelDir "src")
  callProcess "ghc-pkg" [ "init", toFilePath $  soPath </> $(mkRelDir "pkgdb") ]
  mkdirp $ soPath </> $(mkRelDir "install")
  mkdirp $ soPath </> $(mkRelDir "dist")
