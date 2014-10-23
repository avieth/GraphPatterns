{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies #-}

module Data.GraphPatterns.Types (
    Many
  , One
  , ManyToOne
  , OneToMany
  , ManyToMany
  , OneToOne
  , Incoming
  , Outgoing
  , Both
  , EdgeTraversalResult
  , Unique
  , NotUnique
  , DeterminesResult
  , ResultWrapper
  ) where

import Control.Applicative (Applicative)
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)

newtype Many a = Many [a]
  deriving (Functor, Applicative, Monad, Foldable, Traversable)

newtype One a = One (Maybe a)
  deriving (Functor, Applicative, Monad, Foldable, Traversable)

class (Traversable t) => ResultWrapper t

instance ResultWrapper Many

instance ResultWrapper One

data ManyToOne
data OneToMany
data ManyToMany
data OneToOne

data Incoming
data Outgoing
data Both

type family EdgeTraversalResult direction cardinality a where
  EdgeTraversalResult Incoming ManyToOne a = Many a
  EdgeTraversalResult Incoming ManyToMany a = Many a
  EdgeTraversalResult Incoming OneToMany a = One a
  EdgeTraversalResult Incoming OneToOne a = One a
  EdgeTraversalResult Outgoing ManyToOne a = One a
  EdgeTraversalResult Outgoing ManyToMany a = Many a
  EdgeTraversalResult Outgoing OneToMany a = Many a
  EdgeTraversalResult Outgoing OneToOne a = One a
  EdgeTraversalResult Both ManyToOne a = Many a
  EdgeTraversalResult Both ManyToMany a = Many a
  EdgeTraversalResult Both OneToMany a = Many a
  EdgeTraversalResult Both OneToOne a = One a

data Unique
data NotUnique

type family DeterminesResult a b where
  DeterminesResult Unique b = One b
  DeterminesResult NotUnique b = Many b
