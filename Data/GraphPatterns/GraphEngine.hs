{-# LANGUAGE TypeFamilies #-}

module Data.GraphPatterns.GraphEngine (
    GraphEngine(..)
  ) where

import Control.Applicative

-- | Class to describe anything that can be used as a graph engine.
--   We don't want to force it to be in IO, because pure Haskell graph
--   engines should work as well. But, we do demand functor, applicative, monad
--   because without these, it simply wouldn't work. For pure graph engines
--   like graph-core, all you've got to do is wrap them in an appropriate
--   datatype.
class (Functor m, Applicative m, Monad m) => GraphEngine m where
  type Graph m :: *
  type VertexId m :: *
  type EdgeId m :: *
  type EdgeLabel m :: *
  type Vertex m :: *
  type Edge m :: *
  runGraphEngine :: m a -> Graph m -> a
  getVertexById :: VertexId m -> m (Maybe (Vertex m))
  getEdgeById :: EdgeId m -> m (Maybe (Edge m))
  getEdgesOut :: EdgeLabel m -> Vertex m -> m [Edge m]
  getEdgesIn :: EdgeLabel m -> Vertex m -> m [Edge m]
  -- Get the vertex to which an edge goes in (head of edge is here).
  getTargetVertex :: Edge m -> m (Vertex m)
  -- Get the vertex from which an edge goes out (tail of edge is here).
  getSourceVertex :: Edge m -> m (Vertex m)
