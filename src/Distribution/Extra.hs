module Distribution.Extra where

import Data.Version
import Distribution.Package
import Text.ParserCombinators.ReadP (readP_to_S)

dependencyName :: Dependency -> String
dependencyName (Dependency (PackageName name) _) = name

packageIdentifierName :: PackageIdentifier -> String
packageIdentifierName (PackageIdentifier (PackageName name) _) = name

packageIdentifierString :: PackageIdentifier -> String
packageIdentifierString (PackageIdentifier (PackageName name) version) = name ++ "-" ++ showVersion version

maybeParseVersion :: String -> Maybe Version
maybeParseVersion = f . readP_to_S parseVersion
  where f [(x, "")]  = Just x
        f _          = Nothing
