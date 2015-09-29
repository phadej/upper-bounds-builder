{-# LANGUAGE OverloadedStrings #-}
module NoLimits.Types.PackageConfig where

import           Control.Applicative
import           Data.Aeson
import qualified Data.Map as Map
import           Data.Map (Map)
import           Data.Maybe (fromMaybe)

import           NoLimits.Orphans ()

newtype AptPackage = AptPackage { getAptPackage :: String }
  deriving (Eq, Ord, Show)

instance FromJSON AptPackage where
  parseJSON = fmap AptPackage . parseJSON


newtype CliFlag = CliFlag { getCliFlag :: String }
  deriving (Eq, Ord, Show)

instance FromJSON CliFlag where
  parseJSON = fmap CliFlag . parseJSON


data PackageConfig = PackageConfig
  { pcSkip           :: Bool
  , pcAllowNewer     :: Bool
  , pcExtraFlags     :: [CliFlag]
  , pcAptPackages    :: [AptPackage]
  , pcSkipTests      :: Bool
  , pcSkipBenchmarks :: Bool
  , pcExpectTestFail :: Bool
  }
  deriving (Eq, Ord, Show)

defPackageConfig :: PackageConfig
defPackageConfig = PackageConfig False True [] [] False False False

lookupPackageConfig :: String -> Map String PackageConfig -> PackageConfig
lookupPackageConfig name m = fromMaybe defPackageConfig (Map.lookup name m)

instance FromJSON PackageConfig where
  parseJSON = withObject "Package config" $ \obj ->
    PackageConfig <$> obj .:? "skip" .!= False
                  <*> obj .:? "allow-newer" .!= True
                  <*> obj .:? "extra-flags" .!= []
                  <*> obj .:? "apt-packages" .!= []
                  <*> obj .:? "skip-tests" .!= False
                  <*> obj .:? "skip-benchmarks" .!= False
                  <*> obj .:? "expected-test-failure" .!= False

type PackageConfigs = Map String PackageConfig
