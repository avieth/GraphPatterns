{-# LANGUAGE TypeFamilies #-}

module Data.GraphPatterns.GraphEngine (
    GraphEngine(..)
  ) where

import Control.Applicative (Applicative)

-- | Class to describe anything that can be used as a graph engine.
--   We don't want to force it to be in IO, because pure Haskell graph
--   engines should work as well. But, we do demand functor, applicative, monad
--   because without these, it simply wouldn't work. For pure graph engines
--   like graph-core, all you've got to do is wrap them in an appropriate
--   datatype.
--   If your graph is undirected, you need only implement one of
--   getEdgesOut/getEdgesIn. If you implement both, make sure that
--     getEdgesOut v = getEdgesIn v
class (Functor m, Applicative m, Monad m) => GraphEngine m where

  -- | Type of the thing from which we pull our info. Needed only for
  --   runGraphEngine.
  type Graph m :: *

  -- | Type of vertex identifiers.
  type VertexId m :: *

  -- | Type of edge identifiers.
  type EdgeId m :: *

  -- | Type of edge labels. If a given implementation of Graph does not support
  --   this notion, you can use the type Void to ensure that nobody uses this
  --   feature.
  type EdgeLabel m :: *

  -- | Type of vertices.
  type Vertex m :: *

  -- | Type of edges.
  type Edge m :: *

  -- | Must supply a Graph in order to get any information out of the
  --   GraphEngine. This function witnesses that it can be done.
  runGraphEngine :: m a -> Graph m -> a

  -- | Given a VertexId, produce one vertex or none.
  getVertexById :: VertexId m -> m (Maybe (Vertex m))

  -- | Given an EdgeId, produce on edge or none.
  getEdgeById :: EdgeId m -> m (Maybe (Edge m))

  -- | Given a Vertex, produce a list of all Edges outgoing, i.e. with tail
  --   ends at the Vertex.
  --   If your Graph is undirected, this is the same as getEdgesIn
  getEdgesOut :: EdgeLabel m -> Vertex m -> m [Edge m]
  getEdgesOut = getEdgesIn

  -- | Given a Vertex, produce a list of all Edges incoming, i.e. with tail
  --   ends at the Vertex.
  --   If your Graph is undirected, this is the same as getEdgesOut
  getEdgesIn :: EdgeLabel m -> Vertex m -> m [Edge m]
  getEdgesIn = getEdgesOut

  -- | Get the vertex to which an edge goes in (head of edge is here).
  getTargetVertex :: Edge m -> m (Vertex m)

  -- | Get the vertex from which an edge goes out (tail of edge is here).
  getSourceVertex :: Edge m -> m (Vertex m)
