{-# LANGUAGE DeriveDataTypeable #-}
module NoLimits.Plan (plan, alwaysPresent {- TODO: remove -}) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Catch
import qualified Data.Map as Map
import           Data.Traversable (traverse)
import           Data.Typeable
import           Distribution.Extra
import           Distribution.PackDeps
import           NoLimits.Types.PackageConfig

data PackageMissingException = PackageMissingException [String]
  deriving (Show, Typeable)
  
instance Exception PackageMissingException

plan :: MonadThrow m => Newest -> PackageConfigs -> m [DescInfo]
plan newest pkgcfg = 
  let packageInput = Map.keys pkgcfg
  in case traverse (flip Map.lookup newest >=> piDesc) packageInput of
       Nothing  -> throwM $ PackageMissingException packageInput
       Just dis -> do
         let skippable = fst <$> filter (pcSkip . snd) (Map.toList pkgcfg)
         return $ removeUnexisting newest skippable . deepDeps newest $ dis -- Consider all dependencies!

removeUnexisting :: Newest -> [String] -> [DescInfo] -> [DescInfo]
removeUnexisting newest toSkip = map filterDeps . filter pDescInfo
  where pDescInfo di     = pName (packageIdentifierName . diPackage $ di)
        filterDeps di    = di { diDeps     = filter (pSelf di) . filter pDependency $ diDeps di
                              , diLibDeps  = filter (pSelf di) . filter pDependency $ diLibDeps di
                              }
        pSelf di dep     = packageIdentifierName (diPackage di) /= dependencyName dep
        pDependency dep  = pName (dependencyName dep)
        pName name       = Map.member name newest && notElem name toSkip'
        toSkip'          = alwaysPresent ++ toSkip

-- | See <https://ghc.haskell.org/trac/ghc/wiki/Commentary/Libraries/VersionHistory>
alwaysPresent :: [String]
alwaysPresent = words "Cabal Win32 array base bin-package-db binary bytestring containers deepseq directory filepath ghc ghc-prim haskeline hoopl hpc integer-gmp pretty process rts template-haskell terminfo time transformers unix xhtml"
