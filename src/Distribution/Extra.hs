module Distribution.Extra where

import Data.Version
import Distribution.Package

dependencyName :: Dependency -> String
dependencyName (Dependency (PackageName name) _) = name

packageIdentifierName :: PackageIdentifier -> String
packageIdentifierName (PackageIdentifier (PackageName name) _) = name

showPackageIdentifier :: PackageIdentifier -> String
showPackageIdentifier (PackageIdentifier (PackageName name) version) = name ++ "-" ++ showVersion version
