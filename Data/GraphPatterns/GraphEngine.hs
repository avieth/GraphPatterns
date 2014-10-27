{-# LANGUAGE TypeFamilies #-}

module Data.GraphPatterns.GraphEngine (
    GraphEngine(..)
  ) where

import Control.Applicative (Applicative)

-- | A GraphEngine instance proves that a given Monad m implements graph
--   queries.
--
--   It provides
--
--     - A datatype representing the entire graph, instances of which provide
--       a source of information.
--
--     - A datatype representing an edge in the graph.
--
--     - A datatype representing a vertex in the graph.
--
--     - A datatype representing possibly partial vertex information.
--
--     - A datatype representing possibly partial edge information.
--
--   We do not get static verification of graph traversals here; those
--   properties are described at a higher level, in our GraphPatterns DSL,
--   which uses this GraphEngine class as a means of getting source data.
--
class (Functor m, Applicative m, Monad m) => GraphEngine m where

  -- | Type of the thing from which we pull our info. Needed only for
  --   runGraphEngine.
  type EngineGraph m :: *

  -- | Type of vertices.
  data EngineVertex m :: *

  -- | Type of edges.
  data EngineEdge m :: *

  -- | Type of (possibly partial) vertex information.
  data EngineVertexInformation m :: *

  -- | Type of (possibly partial) edge information.
  data EngineEdgeInformation m :: *

  -- | Must supply a Graph in order to get any information out of the
  --   GraphEngine. This function witnesses that it can be done.
  runGraphEngine :: m a -> EngineGraph m -> a

  -- | Get the vertex to which an edge goes in (head of edge is here).
  getTargetVertex :: EngineEdge m -> m (EngineVertex m)

  -- | Get the vertex from which an edge goes out (tail of edge is here).
  getSourceVertex :: EngineEdge m -> m (EngineVertex m)

  -- | Get zero, one, or many EngineVertex from relevant information.
  getVertices :: EngineVertexInformation m -> m [EngineVertex m]

  -- | Get zero, one, or many EngineEdge from relevant information.
  getEdges :: EngineEdgeInformation m -> m [EngineEdge m]

  -- | Given a Vertex, produce a list of all Edges outgoing, i.e. with tail
  --   ends at the Vertex.
  --getEdgesOut
    -- What's at odds here in my mind: should the engine be responsible for
    -- reporting anomalies? We could do it the other way: engine gives back
    -- your edges and our DSL automatically does the anomaly check.
    -- Yeah, same applies to getVertex/getEdge : give some information, return
    -- a list, and have the DSL check whether its an anomaly!
  --  :: (HandlesAnomaly t)
  --  => EngineVertex m
  --  -> m (Either Anomaly (t (EngineEdge m)))
  --
  -- Why do we need this when we have getEdge? Surely we could just give the
  -- adjacency as part of the EngineEdgeInformation, no? We would need, in
  -- order to do this generically, the function
  --
  --   f :: EngineEdgeInformation m -> (EngineVertex m, EngineVertex m)
  --
  -- but that's too much to ask. Not every piece of EngineEdgeInformation
  -- determines in and out vertices.
  --
  -- Instead, we choose to keep EngineEdgeInformation divorced from topology.
  -- An EngineEdgeInformation can make no statement about where that edge lies
  -- in a graph.
  getEdgesOut :: EngineEdgeInformation m -> EngineVertex m -> m [EngineEdge m]

  -- | Given a Vertex, produce a list of all Edges incoming, i.e. with tail
  --   ends at the Vertex.
  getEdgesIn :: EngineEdgeInformation m -> EngineVertex m -> m [EngineEdge m]

  insertVertex :: EngineVertex m -> m (Maybe (EngineVertex m))

  insertEdge
    :: EngineEdge m
    -> EngineVertex m
    -> EngineVertex m
    -> m (Maybe (EngineEdge m))
