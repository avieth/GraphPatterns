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
--     - A datatype representing an edge (vertex) in the graph.
--
--     - A datatype representing an edge (vertex) insertion into the
--       graph, i.e. enough information to insert and then return an
--       edge (vertex)
--
--     - A datatype representing possibly partial edge (vertex)
--       information, i.e. members of which identify 0 or more edges
--       (vertices) in a graph.
--
--   We do not get static verification of graph traversals here; those
--   properties are described at a higher level, in our GraphPatterns DSL,
--   which uses this GraphEngine class as a means of getting source data.
--
class (Functor m, Applicative m, Monad m) => GraphEngine m where

  -- | Type of the thing from which we pull our info. Needed only for
  --   runGraphEngine.
  type EngineGraph m :: *

  -- | Type of vertices. Each member of this type must identify at most one
  --   actual vertex in an EngineGraph.
  data EngineVertex m v :: *
  data EngineEdge m e :: *

  -- | Vertex and Edge instances must provide injections into these types so
  --   that edges and vertices can be inserted into a graph. The GraphEngine
  --   will produce EngineVertex and EngineEdge instances from these via
  --   insertVertex, insertEdge.
  data EngineVertexInsertion m v :: *
  data EngineEdgeInsertion m e :: *

  -- | Type of (possibly partial) vertex information. Each member of this type
  --   must characterize 0 or more vertices in an EngineGraph.
  data EngineVertexInformation m v :: *
  data EngineEdgeInformation m e :: *

  -- | Get the vertex to which an edge goes in (head of edge is here).
  --   Must wrap in Maybe because the input edge could be bogus.
  getTargetVertex :: EngineEdge m e -> m (Maybe (EngineVertex m v))

  -- | Get the vertex from which an edge goes out (tail of edge is here).
  --   Must wrap in Maybe because the input edge could be bogus.
  getSourceVertex :: EngineEdge m e -> m (Maybe (EngineVertex m v))

  -- | Get zero, one, or many EngineVertex from relevant information.
  getVertices :: EngineVertexInformation m v -> m [EngineVertex m v]

  -- | Get zero, one, or many EngineEdge from relevant information.
  getEdges :: EngineEdgeInformation m e -> m [EngineEdge m e]

  -- | Given a Vertex, produce a list of all Edges outgoing, i.e. with tail
  --   ends at the Vertex.
  getEdgesOut :: EngineEdgeInformation m e -> EngineVertex m v -> m [EngineEdge m e]

  -- | Given a Vertex, produce a list of all Edges incoming, i.e. with tail
  --   ends at the Vertex.
  getEdgesIn :: EngineEdgeInformation m e -> EngineVertex m v -> m [EngineEdge m e]

  insertVertex :: EngineVertexInsertion m v -> m (Maybe (EngineVertex m v))

  insertEdge
    :: EngineEdgeInsertion m e
    -> EngineVertex m u
    -> EngineVertex m v
    -> m (Maybe (EngineEdge m e))
