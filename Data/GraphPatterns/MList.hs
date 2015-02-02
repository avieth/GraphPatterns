module Data.GraphPatterns.MList (

    MList

  , fromMList
  , convertToMList
  , toMList

  , mlistEmpty
  , mlistCons
  , mlistAppend
  , mlistConcat
  , mlistTake

  ) where

import Prelude hiding (concat)
import Control.Applicative
import Control.Monad

import Control.Monad.Identity
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

instance (Applicative m) => Applicative (MList m) where
  pure x = MList $ pure (Just (x, MList $ pure Nothing))
  f <*> x = undefined -- ap f x

instance Alternative m => Alternative (MList m) where
  empty = MList empty
  (<|>) = undefined

instance (Applicative m, Monad m) => Monad (MList m) where
  return = pure
  x >>= k = MList $ do
    x' <- runMList x
    case x' of
      Nothing -> return Nothing
      Just (x'', next) -> runMList $ (k x'') `mlistAppend` (next >>= k)

instance (Applicative m, MonadPlus m) => MonadPlus (MList m) where
  mzero = MList mzero
  mplus = undefined

instance MonadTrans MList where
  lift mt = MList $ liftM (\x -> Just (x, mlistEmpty)) mt

mlistAppend :: Monad m => MList m a -> MList m a -> MList m a
mlistAppend first second = MList $ do
  head <- runMList first
  case head of
    Nothing -> runMList second
    Just (h, rest) -> return $ Just (h, mlistAppend rest second)

mlistEmpty :: Monad m => MList m a
mlistEmpty = MList $ return Nothing

mlistCons :: Monad m => m a -> MList m a -> MList m a
mlistCons x rest = MList $ do
  x' <- x
  return $ Just (x', rest)

convertToMList :: Monad m => m [a] -> MList m a
convertToMList val = MList $ do
  list <- val
  case list of
    [] -> return Nothing
    (x : xs) -> return $ Just (x, convertToMList (return xs))

toMList :: Monad m => [a] -> MList m a
--toMList [] = MList $ return Nothing
--toMList (x:xs) = MList $ return (Just (x, toMList xs))
toMList [] = mlistEmpty
toMList (x:xs) = mlistCons (return x) (toMList xs)

fromMList :: (Applicative m, Monad m) => MList m a -> m [a]
fromMList x = do
  head <- runMList x
  case head of
    Nothing -> return []
    Just (x', rest) -> (:) <$> return x' <*> fromMList rest

mlistConcat :: Monad m => MList m (MList m a) -> MList m a
mlistConcat x = MList $ do
  head <- runMList x
  case head of
    Nothing -> return Nothing
    Just (anMList, rest) -> runMList $ mlistAppend anMList (mlistConcat rest)

mlistTake :: Monad m => Int -> MList m a -> m [a]
mlistTake n x = case n of
  0 -> return []
  n -> do
    head <- runMList x
    case head of
      Nothing -> return []
      Just (x', rest) -> liftM ((:) x') (mlistTake (n-1) rest)
