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

  toEngineEdgeInsertion :: e -> EngineEdgeInsertion m e
  fromEngineEdge :: EngineEdge m e -> Maybe e

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
    -> EngineEdgeInformation m e

-- With an instance of this class we assert that a type d indicates some local
-- edge properties at vertices of type v, for one type of edge e.
--
--   - The direction of that edge.
--   - A projection onto EngineEdgeInformation for the given GraphEngine, so
--     that edges at a Vertex of the given type can be filtered to what is
--     requested.
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
  , v ~ IfBoth (EdgeDirection m v e determiner) (EdgeSource m e) v
  , v ~ IfBoth (EdgeDirection m v e determiner) (EdgeTarget m e) v
    -- ^ The above two conjuncts fix EdgeSource m e ~ EdgeTarget m e just in
    --   case the direction is Both (if it's not both, they degenerate to
    --   v ~ v
  ) => DeterminesLocalEdge m v e determiner where

  -- Must indicate direction. Must be one of Out, In, Both.
  -- In means "goes into a vertex of type v" and out means "goes out of
  -- a vertex of type v" so in the former, the edge would have its tip at a
  -- vertex of this type.
  type EdgeDirection m v e determiner :: *

  toEngineEdgeInformationLocal
    :: Proxy m
    -- I thought using a Proxy would be better, but I wasn't able to use that
    -- to disambiguate... I was probably doing something wrong.
    -> Proxy v
    -> Proxy e
    -> determiner
    -> EngineEdgeInformation m e

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
