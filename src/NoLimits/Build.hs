{-# LANGUAGE TemplateHaskell #-}
module NoLimits.Build where

import Development.Shake
import Distribution.Package
import Distribution.PackDeps
import Path

import NoLimits.Paths

data BuildStep = StepGet
               | StepConfigure
               | StepBuild
               | StepCopy
               | StepRegister
               | StepTestConfigure
               | StepTestBuild
               | StepTestRun
               | StepBenchConfigure
               | StepBenchBuild
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

stepSuffix :: BuildStep -> String
stepSuffix StepGet = "get"
stepSuffix _ = error "stepSuffix: implement me"

stepGetAction :: Path Abs Dir -> DescInfo -> Action ()
stepGetAction rootDir di = return ()
  where srcDir = rootDir </> $(mkRelDir "src")
