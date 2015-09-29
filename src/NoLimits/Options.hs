module NoLimits.Options where

import Data.Bifunctor
import Options.Applicative
import Path

import NoLimits.Types

pathAbsDirArgument :: Mod ArgumentFields (Path Abs Dir) -> Parser (Path Abs Dir)
pathAbsDirArgument = argument readm
  where readm = eitherReader $ first show . parseAbsDir
  
setupOptsParser :: Parser SetupOpts
setupOptsParser = SetupOpts <$> pathAbsDirArgument (metavar "BUILDDIR" <> help "Destination directory")
