{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

-- TODO: skip-benchmarks
-- TODO: expected-test-failure

import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.Writer.Strict
import           Data.Bifunctor
import           Data.Binary.Tagged.Extra
import           Data.Foldable
import           Data.Function (on)
import           Data.List as L (sort, sortBy, partition)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.Monoid
import           Data.Traversable
import           Data.Version
import           Data.Yaml.Extra
import           Distribution.Extra
import           Distribution.Package
import           Options.Applicative as O
import           Path
import           Prelude hiding (elem, all, notElem)
import           System.FilePath hiding ((</>))

import           Distribution.PackDeps

import           NoLimits.Build
import           NoLimits.CheckDeps
import           NoLimits.Options
import           NoLimits.Plan
import           NoLimits.Setup
import           NoLimits.Types
import           NoLimits.Types.BuildInfo

import           Debug.Trace

optionParser :: O.Parser (IO ())
optionParser = subparser $ mconcat
  [ O.command "setup" $ info (helper <*> (cmdSetup <$> setupOptsParser)) $ progDesc "Run setup commands"
  , O.command "build" $ info (helper <*> (cmdBuild <$> setupOptsParser)) $ progDesc "Run the build"
  ]

main :: IO ()
main = join (execParser opts)
  where
    opts = info (helper <*> optionParser)
      ( fullDesc
     <> header "upper-bounds-builder - building without limits" )

cmdBuild :: SetupOpts -> IO ()
cmdBuild SetupOpts {..} = do
  pkgcfg <- decodeFileThrow "config.yaml" :: IO PackageConfigs
  newest <- cached (soPath </> $(mkRelFile "cache/newest")) loadNewest
  alldeps <- plan newest pkgcfg
  makeBuild True benv $ sortBy (compare `on` biPackage) alldeps
  where
    benv = BuildEnv
      { beBuildRootDir = soPath
      , beSourceDir    = soPath </> $(mkRelDir "src")
      }
