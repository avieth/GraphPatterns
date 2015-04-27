module Data.GraphPatterns.MList (

    MList

  , ml_tolist
  , ml_fromlist
  , ml_fromlist'

  , ml_singleton
  , ml_empty
  , ml_cons
  , ml_append
  , ml_concat
  , ml_take

  ) where

import Prelude hiding (concat)
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State

-- | We begin with a special monad-list type, as seen in the ListT done right
--   alternative http://www.haskell.org/haskellwiki/ListT_done_right_alternative
newtype MList m a = MList {
    runMList :: m (Maybe (a, MList m a))
  }

instance Functor m => Functor (MList m) where
  fmap f = MList . (fmap . fmap) f' . runMList
    where f' (x, next) = (f x, fmap f next)

instance (Applicative m, Monad m) => Applicative (MList m) where
  pure = ml_singleton
  fs <*> xs = ml_concat $ fmap (\f -> fmap f xs) fs

instance (Applicative m, Monad m) => Alternative (MList m) where
  empty = ml_empty
  (<|>) = ml_append

instance (Applicative m, Monad m) => Monad (MList m) where
  return = pure
  x >>= k = MList $ do
    x' <- runMList x
    case x' of
      Nothing -> return Nothing
      Just (x'', next) -> runMList $ (k x'') `ml_append` (next >>= k)

instance (Applicative m, Monad m) => MonadPlus (MList m) where
  mzero = ml_empty
  mplus = ml_append

instance MonadTrans MList where
  lift mt = MList $ liftM (\x -> Just (x, ml_empty)) mt

ml_empty :: Monad m => MList m a
ml_empty = MList $ return Nothing

ml_singleton :: Monad m => a -> MList m a
ml_singleton x = MList $ return (Just (x, ml_empty))

ml_append :: Monad m => MList m a -> MList m a -> MList m a
ml_append first second = MList $ do
    head <- runMList first
    case head of
        Nothing -> runMList second
        Just (h, rest) -> return $ Just (h, ml_append rest second)

ml_cons :: (Functor m, Monad m) => m a -> MList m a -> MList m a
ml_cons x rest = MList $ fmap (\y -> Just (y, rest)) x

ml_concat :: Monad m => MList m (MList m a) -> MList m a
ml_concat x = MList $ do
  head <- runMList x
  case head of
    Nothing -> return Nothing
    Just (anMList, rest) -> runMList $ ml_append anMList (ml_concat rest)

ml_take :: Monad m => Int -> MList m a -> m [a]
ml_take n x = case n of
  0 -> return []
  n -> do
    head <- runMList x
    case head of
      Nothing -> return []
      Just (x', rest) -> liftM ((:) x') (ml_take (n-1) rest)

ml_fromlist :: (Applicative m, Monad m) => [a] -> MList m a
ml_fromlist [] = ml_empty
ml_fromlist (x:xs) = ml_cons (return x) (ml_fromlist xs)

ml_fromlist' :: Monad m => m [a] -> MList m a
ml_fromlist' val = MList $ do
  list <- val
  case list of
    [] -> return Nothing
    (x : xs) -> return $ Just (x, ml_fromlist' (return xs))

-- | Get a list, forcing all monadic effects.
ml_tolist :: (Applicative m, Monad m) => MList m a -> m [a]
ml_tolist x = do
  head <- runMList x
  case head of
    Nothing -> return []
    Just (x', rest) -> (:) <$> return x' <*> ml_tolist rest
