{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.GraphPatterns.Edge (
    Edge(..)
  , EdgeRelated(..)
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

-- | With an instance of this class we assert simply that a given type e embeds
--   into EngineEdge m for some GraphEngine m.
class (GraphEngine m) => Edge m e where
  toEngineEdgeInsertion :: e -> EngineEdgeInsertion m e
  fromEngineEdge :: EngineEdge m e -> Maybe e

-- | With an instance of this class we assert that a given type d can be used
--   to pick out a bunch of edges in a graph without using topological
--   properties (so we stick to edge-intrinsic properties), and we indicate
--   whether it picks out at most one edge or possibly more.
class (GraphEngine m, Edge m e) => DeterminesEdge m e determiner where
  type EdgeUniqueness m e determiner :: *
  -- ^ Must be one of Unique, NotUnique
  toEngineEdgeInformation :: determiner -> EngineEdgeInformation m e

-- | An edge of type e links source type s to target type t.
class EdgeRelated e s t where
  type EdgeCardinalitySource e s t :: *
  -- ^ Multi or Single. Single means that for any fixed s, there is
  --   at most one edge of this type leaving it.
  type EdgeCardinalityTarget e s t :: *
  -- ^ Multi or Single. Single means that for any fixed t, there is
  --   at most one edge of this type entering it.

-- | With an instance of this class we assert that a type identifies some local
--   edges at vertices of type v.
--
--   - The direction of that edge.
--   - A projection onto EngineEdgeInformation for the given GraphEngine, so
--     that edges at a Vertex of the given type can be filtered to what is
--     requested.
class 
  ( GraphEngine m
  , Edge m e
  ) => DeterminesLocalEdge m e s t determiner where

  toEngineEdgeInformationLocal
    :: Proxy s
    -> Proxy t
    -> determiner
    -> EngineEdgeInformation m e
