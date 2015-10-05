{-# LANGUAGE OverloadedStrings #-}
module NoLimits.Types.BuildInfo (BuildInfo(..), BuildConfig(..)) where

import           Control.Applicative
import           Data.Aeson.Compat
import           Data.Aeson.Types hiding ((.:?))
import           Data.Semigroup
import           Distribution.Package
import qualified System.Info as Info

data BuildConfig = BuildConfig
  { bcSkipTest  :: Bool
  , bcSkipBench :: Bool
  , bcExtraFlags :: [String]
  }
  deriving (Eq, Ord, Show)

instance Semigroup BuildConfig where
  a <> b = BuildConfig { bcSkipTest  = bcSkipTest a || bcSkipTest b 
                       , bcSkipBench = bcSkipBench a || bcSkipBench b
                       , bcExtraFlags = bcExtraFlags a <> bcExtraFlags b
                       }

instance Monoid BuildConfig where
  mempty = BuildConfig False False []
  mappend = (<>)

instance FromJSON BuildConfig where
  parseJSON = withObject "BuildConfig" $ \obj ->
    (<>) <$> parseJSONBuildConfig obj
         <*> (getBC <$> obj .:? sys .!= BC mempty)
    where sys = case Info.os of
                  "darwin"  -> "macosx"
                  "mingw32" -> "windows"
                  "linux"   -> "linux"
                  _         -> "otheros"

parseJSONBuildConfig :: Object -> Parser BuildConfig
parseJSONBuildConfig obj =
  BuildConfig <$> obj .:? "skip-tests" .!= False
              <*> obj .:? "skip-benchmarks" .!= False
              <*> obj .:? "extra-flags" .!= []

newtype BC = BC { getBC :: BuildConfig }

instance FromJSON BC where
  parseJSON = fmap BC . withObject "BuildConfig part" parseJSONBuildConfig

data BuildInfo = BuildInfo
  { biPackage   :: PackageIdentifier
  , biLibDeps   :: [PackageIdentifier]
  , biTestDeps  :: [PackageIdentifier]
  , biBenchDeps :: [PackageIdentifier]
  , biConfig    :: BuildConfig
  }
  deriving (Eq, Ord, Show)