{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.GraphPatterns.Edge (

    Edge(..)
  , DeterminesEdge(..)
  , DeterminesLocalEdge(..)

  ) where

import Data.Proxy
import Data.GraphPatterns.GraphEngine
import Data.GraphPatterns.Vertex
import Data.GraphPatterns.Types

-- | A Haskell datatype can be treated as an Edge in some GraphEngine.
class
    ( GraphEngine g
    , Vertex g (EdgeTarget g e)
    , Vertex g (EdgeSource g e)
    ) => Edge g e
  where
    type EdgeTarget g e :: *
    type EdgeSource g e :: *
    toEngineEdgeInsertion :: Proxy g -> e -> EngineEdgeInsertion g e
    fromEngineEdge :: Proxy g -> Proxy e -> EngineEdge g e -> GraphEngineMonad g e

-- | With an instance of this class we assert that a given type d can be used
--   to pick out a bunch of edges in a graph without using topological
--   properties (so we stick to edge-intrinsic properties).
class (GraphEngine g, Edge g e) => DeterminesEdge g e determiner where
  toEngineEdgeInformation
    :: Proxy g
    -> Proxy e
    -> determiner
    -> EngineEdgeInformation g e

-- | With an instance of this class we assert that a type identifies some local
--   edges at vertices of type v.
class (Edge g e) => DeterminesLocalEdge g e determiner where
    toEngineEdgeInformationLocal
      :: Proxy g
      -> Proxy e
      -> determiner
      -> EngineEdgeInformation g e
