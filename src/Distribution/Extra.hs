module Distribution.Extra where

import Data.Version
import Distribution.Package
import Text.ParserCombinators.ReadP (readP_to_S)

class HasPackageName a where
  getPackageName :: a -> PackageName
  
getPackageNameString :: HasPackageName a =>  a-> String
getPackageNameString x = case getPackageName x of
                           (PackageName name) -> name

instance HasPackageName PackageName where
  getPackageName = id

instance HasPackageName Dependency where
  getPackageName (Dependency name _) = name   

instance HasPackageName PackageIdentifier where
  getPackageName (PackageIdentifier name _) = name

packageIdentifierString :: PackageIdentifier -> String
packageIdentifierString (PackageIdentifier (PackageName name) version) = name ++ "-" ++ showVersion version

maybeParseVersion :: String -> Maybe Version
maybeParseVersion = f . readP_to_S parseVersion
  where f [(x, "")]  = Just x
        f _          = Nothing
