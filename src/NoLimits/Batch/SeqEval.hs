{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module NoLimits.Batch.SeqEval (evalBatch) where

import           Control.Applicative
import           Control.Monad (when)
import           Control.Monad.Catch
import           Control.Monad.Logger
import           Data.Data (Typeable)
import           Data.Either (partitionEithers)
import qualified Data.List as L
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Monoid
import qualified Data.Set as Set
import           Data.Traversable
import qualified Data.Text as T

import           NoLimits.Batch.Job

newtype StalledBatchException = StalledBatchException String
  deriving (Show, Typeable)
  
instance Exception StalledBatchException

evalBatch :: (Applicative m, MonadThrow m, MonadLogger m, Ord t, Show t) => [Job t m a] -> m (Map t a)
evalBatch = evalBatch' Map.empty

evalBatch' :: forall t m a. (Applicative m, MonadThrow m, MonadLogger m, Ord t, Show t) => Map t a -> [Job t m a] -> m (Map t a)
evalBatch' done []  = return done
evalBatch' done jobs = do
  $logDebug $ "Jobs left: " <> T.pack (show (L.length jobs))
  let (next, blocked) = partitionEithers $ L.map p jobs
  when (L.null next) $ throwM $ StalledBatchException (let j = head blocked
                                                       in show (jobResult j) <> " <- " <> show (jobPrereq j))
  results <- traverse (uncurry runJob) next
  evalBatch' (Map.union done $ Map.fromList results) blocked
  where 
    -- partition jobs into
    --   * runnable, with projection of 'done' map 
    --   * blocked jobs
    p :: Job t m a -> Either (Job t m a, Map t a) (Job t m a)
    p job =
      case traverse l . Set.toList . jobPrereq $ job of
        Nothing -> Right job
        Just ms -> Left (job, Map.fromList ms)
      where
        l :: t -> Maybe (t, a)
        l tag = (tag,) <$> Map.lookup tag done

    runJob :: Job t m a -> Map t a -> m (t, a)
    runJob job done' = (jobResult job,) <$> jobAction job done'

{-
testEvalBatch :: WriterT [[(Int, Int)]] Maybe (Map Int Int)
testEvalBatch = evalBatch (mkJob <$> [0..9])
  where mkJob n = Job { jobPrereq = Set.fromList (filter (p n) [0..n-1]) 
                      , jobResult = n
                      , jobAction = \m -> do tell [Map.toList m]
                                             return (1 + sum (Map.elems m))
                      }
        p :: Int -> Int -> Bool
        p n | even n    = even
            | otherwise = odd        
-}
