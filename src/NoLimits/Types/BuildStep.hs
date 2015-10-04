module NoLimits.Types.BuildStep where

import Control.Applicative
import Data.Aeson.Compat
import Data.Monoid
import Data.Text as T

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

buildStepString :: BuildStep -> String
buildStepString StepGet = "get"
buildStepString _ = error "stepSuffix: implement me"

buildStepLookupList :: [(String, BuildStep)]
buildStepLookupList = fmap f [minBound..maxBound]
  where f step = (buildStepString step, step)

instance ToJSON BuildStep where
  toJSON = toJSON . buildStepString

instance FromJSON BuildStep where
  parseJSON = withText "BuildStep" $ \t ->
    let s = T.unpack t
    in case lookup s buildStepLookupList of
         Just step -> pure step
         Nothing   -> fail $ "Invalid BuildStep " <> s

