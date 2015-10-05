{-# LANGUAGE OverloadedStrings #-}
module NoLimits.Types.PackageConfig where

import           Control.Applicative
import           Data.Aeson
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.Semigroup

import           NoLimits.Orphans ()
import           NoLimits.Types.BuildInfo

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
  , pcBuildConfig    :: BuildConfig
  --  , pcAptPackages    :: [AptPackage]
  }
  deriving (Eq, Ord, Show)

defPackageConfig :: PackageConfig
defPackageConfig = PackageConfig False mempty

lookupPackageConfig :: String -> Map String PackageConfig -> PackageConfig
lookupPackageConfig name m = fromMaybe defPackageConfig (Map.lookup name m)

instance FromJSON PackageConfig where
  parseJSON = withObject "Package config" $ \obj ->
    PackageConfig <$> obj .:? "skip" .!= False
                  <*> parseJSON (Object obj)

type PackageConfigs = Map String PackageConfig
