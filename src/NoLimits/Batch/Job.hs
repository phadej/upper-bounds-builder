{-# LANGUAGE RankNTypes #-}
module NoLimits.Batch.Job where

import Data.Set (Set)
import Data.Map (Map)

-- |
--    * 't' tag
--    * 'm' monad
--    * 'a' result
--
-- /Invariant:/ 'jobAction' is called with 'Map' containing all keys from 'jobPrereq'.
data Job t m a = Job
  { jobPrereq :: Set t
  , jobResult :: t
  , jobAction :: Map t a -> m a
  }

transformJobAction :: (forall a. m a -> n a) -> Job t m a -> Job t n a
transformJobAction f job@Job { jobAction = action } = job { jobAction = f . action }
