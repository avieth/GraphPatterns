{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.GraphPatterns.Edge (
    Edge(..)
  , DeterminesEdge(..)
  , DeterminesLocalEdge(..)
  , Out
  , In
  , IfIn
  , IfOut
  , Multi
  , Single
  , IfSingle
  , IfMulti
  ) where

import Data.Proxy

import Data.GraphPatterns.GraphEngine
import Data.GraphPatterns.Vertex
import Data.GraphPatterns.Types

-- With an instance of this class we assert simply that a given type e embeds
-- into EngineEdge m for some GraphEngine m.
class
  ( GraphEngine m
  , Vertex m (EdgeTarget m e)
  , Vertex m (EdgeSource m e)
  ) => Edge m e where

  -- NB we really cannot use data families here; we _must_ have noninjectivity,
  -- because different edges can land on the same vertex types.
  -- This complicates type inference.
  type EdgeSource m e :: *
  type EdgeTarget m e :: *

  type EdgeCardinalitySource m e :: *
  -- ^ Single or Multi. This describes how many (at most 1 or arbitrarily many)
  -- edges of this type can be out-incident with an EdgeSource. If it's single
  -- then we know 
  type EdgeCardinalityTarget m e :: *

  -- ^ How many targets? Multi or Single?
  toEngineEdge :: Proxy m -> e -> EngineEdge m
  -- ^ We give the first parameter so that we can disambiguate.
  fromEngineEdge :: Proxy m -> EngineEdge m -> Maybe e
  -- ^ Wrap in maybe because the embedding may not be a bijection.

-- With an instance of this class we assert that a given type d can be used
-- to pick out a bunch of edges in a graph without using topological properties
-- (so we stick to edge-intrinsic properties), and we indicate whether it picks
-- out at most one edge or possibly more.
class (GraphEngine m, Edge m e) => DeterminesEdge m d e where
  -- Must be one of Unique, NotUnique
  type EdgeUniqueness m d e :: *
  toEngineEdgeInformation
    :: Proxy m
    -> Proxy e
    -> d
    -> EngineEdgeInformation m

-- With an instance of this class we assert that a type d indicates some local
-- edge properties at vertices of type v, for one type of edge e.
--
--   - The direction of that edge. This means that at a given vertex, a given
--     edge type can only go in one direction, out or in. But that's
--     unacceptable! TODO
--
-- Ok, step back. What we want to facilitate here is the use of a type T to
-- indicate local edge type constraints.
class 
  ( GraphEngine m
  , Edge m e
  , Vertex m v
    -- We must somehow constrain the vertex type using the direction and the
    -- edge's source/target types! Looks like we can't do that here, though,
    -- can we? If we did it in a separate type family, then we could...
    -- Ah, it does work!
    -- Here we state that the vertex must line up with the edge's declarations
    -- given the direction.
    -- NB it therefore asserts the vertex must line up with at least one of the
    -- edge's ends. Perhaps subtle, but important!
  , v ~ IfOut (EdgeDirection m v e determiner) (EdgeSource m e) (EdgeTarget m e)
  ) => DeterminesLocalEdge m v e determiner where

  -- Must indicate direction. Must be one of Out, In
  -- In means "goes into a vertex of type v" and out means "goes out of
  -- a vertex of type v" so in the former, the edge would have its tip at a
  -- vertex of this type.
  type EdgeDirection m v e determiner :: *
  -- Must indicate cardinality. One of Multi, Single
  -- NO we don't put this here, it's recoverable from the Edge instance and
  -- the direction!
  --type EdgeCardinality m v e determiner :: *
  toEngineEdgeInformationLocal
    :: Proxy m
    -- I thought using a Proxy would be better, but I wasn't able to use that
    -- to disambiguate... I was probably doing something wrong.
    -> Proxy v
    -> Proxy e
    -> determiner
    -> EngineEdgeInformation m

-- TBD any way to do this without getting duplicates, without requiring
-- flexible instances?
{-
instance
  (
    DeterminesEdge m d e
  , v ~ EdgeTarget m e
  ) => DeterminesLocalEdge m v e d where
  type EdgeDirection m v e d = In
  toEngineEdgeInformationLocal x _ y z = toEngineEdgeInformation x y z

instance
  (
    DeterminesEdge m d e
  , v ~ EdgeSource m e
  ) => DeterminesLocalEdge m v e d where
  type EdgeDirection m v e d = Out
  toEngineEdgeInformationLocal x _ y z = toEngineEdgeInformation x y z
-}

data Out
data In

type family IfIn d a b :: * where
  IfIn In a b = a
  IfIn Out a b = b

type family IfOut d a b :: * where
  IfOut Out a b = a
  IfOut In a b = b

data Multi
data Single

type family IfMulti d a b :: * where
  IfMulti Multi a b = a
  IfMulti Single a b = b

type family IfSingle d a b :: * where
  IfSingle Single a b = a
  IfSingle Multi a b = b
