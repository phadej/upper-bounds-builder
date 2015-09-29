{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGuAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module NoLimits.Orphans () where

import Data.Aeson
import Data.Binary
import Data.Binary.Tagged hiding (Version)
import Data.Foldable
import Distribution.Package
import Distribution.PackageDescription
import Distribution.Version
import GHC.Generics
import GHC.TypeLits

instance FromJSON PackageName where
  parseJSON = fmap PackageName . parseJSON

deriving instance Foldable (CondTree v c)

deriving instance Generic Version
deriving instance Generic VersionRange
deriving instance Generic Dependency
deriving instance Generic PackageName
deriving instance Generic PackageIdentifier

instance HasStructuralInfo Version
instance HasStructuralInfo VersionRange where structuralInfo _ = NominalType "VersionRange"
instance HasStructuralInfo Dependency where structuralInfo _ = NominalType "Dependency"
instance HasStructuralInfo PackageName
instance HasStructuralInfo PackageIdentifier

instance Binary Version
instance Binary VersionRange
instance Binary Dependency
instance Binary PackageName
instance Binary PackageIdentifier

instance HasSemanticVersion Version
instance (HasSemanticVersion a, HasSemanticVersion b, KnownNat (SemanticVersion (a, b))) => HasSemanticVersion (a, b) where
  type SemanticVersion (a, b) = Interleave (SemanticVersion a) (SemanticVersion b)
