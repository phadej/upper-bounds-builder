{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
module NoLimits.Setup where

import Path
import System.Directory

import NoLimits.Types

mkdirp :: Path Abs Dir -> IO ()
mkdirp = createDirectoryIfMissing True . toFilePath

cmdSetup :: SetupOpts -> IO ()
cmdSetup SetupOpts {..} = do
  mkdirp $ soPath </> $(mkRelDir "cache")
  mkdirp $ soPath </> $(mkRelDir "log")
  mkdirp $ soPath </> $(mkRelDir "src")
  mkdirp $ soPath </> $(mkRelDir "pkgconf")
  mkdirp $ soPath </> $(mkRelDir "dist/build")
  mkdirp $ soPath </> $(mkRelDir "dist/test")
  mkdirp $ soPath </> $(mkRelDir "dist/bench")
