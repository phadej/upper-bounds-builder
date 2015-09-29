{-# LANGUAGE FlexibleContexts #-}
module NoLimits.CheckDeps (checkDepsCli) where

import Control.Applicative
import Control.Monad.Writer.Class
import Control.Monad.Writer.Strict
import Data.DList as DList
import Data.Foldable
import Data.Version
import Distribution.Package
import Distribution.PackDeps

type CheckDeps = Newest -> DescInfo -> (PackageName, Version, CheckDepsRes)

-- TODO: pre AMP
checkDepsCliOne :: (Applicative m, MonadWriter (DList String) m) => CheckDeps -> Newest -> DescInfo -> m ()
checkDepsCliOne cd newest di =
    case cd newest di of
        (_, _, AllNewest) -> return ()
        (PackageName pn, v, WontAccept p _) -> do
           tell $ DList.singleton $ mconcat
             [ pn
             , "-"
             , showVersion v
             , ": Cannot accept the following packages"
             ]
           flip traverse_ p $ \(x, y) -> tell . DList.singleton $ "- " <> x <> "-" <> y

checkDepsCli :: Foldable f => Newest -> f DescInfo -> String
checkDepsCli newest dis = Prelude.unlines $ DList.toList $ execWriter $ traverse_ (checkDepsCliOne checkDeps newest) dis
