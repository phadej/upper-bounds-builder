{-# LANGUAGE DeriveDataTypeable #-}
module NoLimits.Plan (plan, alwaysPresent {- TODO: remove -}) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Catch
import qualified Data.Map as Map
import           Data.Maybe (mapMaybe)
import           Data.Traversable (traverse)
import           Data.Typeable
import           Distribution.Extra
import           Distribution.PackDeps
import           Distribution.Package
import           NoLimits.Types.BuildInfo
import           NoLimits.Types.PackageConfig

data PackageMissingException = PackageMissingException [String]
  deriving (Show, Typeable)
  
instance Exception PackageMissingException

plan :: MonadThrow m => Newest -> PackageConfigs -> m [BuildInfo]
plan newest pkgcfg = 
  let packageInput = Map.keys pkgcfg
  in case traverse (flip Map.lookup newest >=> piDesc) packageInput of
       Nothing  -> throwM $ PackageMissingException packageInput
       Just dis -> do
         let skippable = PackageName . fst <$> filter (pcSkip . snd) (Map.toList pkgcfg)
         return . removeUnexisting newest skippable . deepDeps newest $ dis -- Consider all dependencies!

removeUnexisting :: Newest        -- ^ Package database
                 -> [PackageName] -- ^ Packages tagged skippable
                 -> [DescInfo]    -- ^ Package descriptions
                 -> [BuildInfo]
removeUnexisting newest toSkip = map toBuildInfo . filter pDescInfo
  where pDescInfo di     = pName (packageIdentifierName . diPackage $ di)
        
        toBuildInfo :: DescInfo -> BuildInfo
        toBuildInfo di = BuildInfo
          { biPackage   = diPackage di
          , biLibDeps   = mapMaybe lookupNewestDependency . diLibDeps $ di
          , biTestDeps  = [] -- TODO:
          , biBenchDeps = []
          }

        lookupNewestDependency :: Dependency -> Maybe PackageIdentifier
        lookupNewestDependency (Dependency name@(PackageName name') _)
          | name `elem` toSkip'  = Nothing
          | otherwise            = diPackage <$> (Map.lookup name' newest >>= piDesc)

        pDependency dep  = pName (dependencyName dep)
        pName name       = Map.member name newest && notElem (PackageName name) toSkip'
        toSkip'          = alwaysPresent ++ toSkip

-- | See <https://ghc.haskell.org/trac/ghc/wiki/Commentary/Libraries/VersionHistory>
alwaysPresent :: [PackageName]
alwaysPresent = PackageName <$> words "Cabal Win32 array base bin-package-db binary bytestring containers deepseq directory filepath ghc ghc-prim haskeline hoopl hpc integer-gmp pretty process rts template-haskell terminfo time transformers unix xhtml"
