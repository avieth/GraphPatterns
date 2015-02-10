{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.TypeRelation (
    Smaller(..)
  , Bigger(..)
  ) where

-- | s is contained in t, and is therefore a smaller type than t.
class Smaller s t where
  inject :: s -> t

-- | s contains t, and is therefore a bigger type than t.
class Bigger t s where
  project :: t -> Maybe s

instance Smaller t t where
  inject = id

instance Bigger t t where
  project = Just

instance Smaller t (Either t s) where
  inject = Left

instance Smaller t (Either s t) where
  inject = Right

instance Bigger (Either s t) s where
  project x = case x of
    Left y -> Just y
    Right _ -> Nothing

instance Bigger (Either s t) t where
  project x = case x of
    Left _ -> Nothing
    Right y -> Just y

instance Bigger (s, t) s where
  project = Just . fst

instance Bigger (s, t) t where
  project = Just . snd
