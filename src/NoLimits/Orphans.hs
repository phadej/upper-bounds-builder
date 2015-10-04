{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGuAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module NoLimits.Orphans () where

import Control.Applicative
import Data.Aeson.Compat
import Data.Binary
import Data.Binary.Tagged hiding (Version)
import Data.Foldable
import Data.Monoid
import Data.Text as T
import Data.Version
import Distribution.Extra
import Distribution.Package
import Distribution.PackageDescription
import Distribution.Version
import GHC.Generics
import GHC.TypeLits

deriving instance Foldable (CondTree v c)

deriving instance Generic Version
deriving instance Generic VersionRange
deriving instance Generic Dependency
deriving instance Generic PackageName
deriving instance Generic PackageIdentifier

instance Binary Version
instance Binary VersionRange
instance Binary Dependency
instance Binary PackageName
instance Binary PackageIdentifier

instance HasStructuralInfo Version where structuralInfo _ = NominalType "Version"
instance HasStructuralInfo VersionRange where structuralInfo _ = NominalType "VersionRange"
instance HasStructuralInfo Dependency where structuralInfo _ = NominalType "Dependency"
instance HasStructuralInfo PackageName
instance HasStructuralInfo PackageIdentifier

instance HasSemanticVersion Version
instance (HasSemanticVersion a, HasSemanticVersion b, KnownNat (SemanticVersion (a, b))) => HasSemanticVersion (a, b) where
  type SemanticVersion (a, b) = Interleave (SemanticVersion a) (SemanticVersion b)

instance ToJSON PackageIdentifier where
  toJSON (PackageIdentifier name ver) = object [ "name" .= name, "version" .= ver ]

instance FromJSON PackageIdentifier where
  parseJSON = withObject "PackageIdentifier" $ \obj ->
    PackageIdentifier <$> obj .: "name"
                      <*> obj .: "version"

instance ToJSON PackageName where
  toJSON (PackageName name) = toJSON name

instance FromJSON PackageName where
  parseJSON = fmap PackageName . parseJSON

instance ToJSON Version where
  toJSON v = toJSON (showVersion v)

instance FromJSON Version where
  parseJSON = withText "Version" $ \t ->
    let s = T.unpack t
    in case maybeParseVersion s of
         Just v   -> return v
         Nothing  -> fail $ "Invalid version string: " <> s
