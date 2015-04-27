{-|
Module      : Data.GraphPatterns.GraphEngine
Description : Definition of the GraphEngine class.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.GraphPatterns.GraphEngine (

    GraphEngine(..)

  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.TransT

class  ( Functor (Result m)
       , Applicative (Result m)
       , Monad (Result m)
       , CommuteM (Result m)
       , Functor (Effect m)
       , Applicative (Effect m)
       , Monad (Effect m)
       )
    => GraphEngine m where

  -- | Type of the thing which represents the graph data.
  --   TBD necessary? Should remove I think.
  type EngineGraph m :: *

  -- | TODO explain these
  type Effect m :: * -> *
  type Result m :: * -> *

  -- | Type of vertices. Each member of this type must identify at most one
  --   vertex in an EngineGraph.
  data EngineVertex m v :: *

  -- | Type of edges. Each member of this type must identify at most one
  --   edge in an EngineGraph.
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

  -- | Get the terminal vertex of an edge.
  getTargetVertex :: EngineEdge m e -> (Effect m) (Result m (EngineVertex m v))

  -- | Get the source vertex of an edge.
  getSourceVertex :: EngineEdge m e -> (Effect m) (Result m (EngineVertex m v))

  -- | Get zero, one, or many @EngineVertex@s from relevant information.
  getVertices :: EngineVertexInformation m v -> (Effect m) [(Result m) (EngineVertex m v)]

  -- | Get zero, one, or many @EngineEdge@s from relevant information.
  getEdges :: EngineEdgeInformation m e -> (Effect m) [(Result m) (EngineEdge m e)]

  -- | Get all edges initiating from a vertex, and satisfying some
  --   characterization.
  getEdgesOut :: EngineEdgeInformation m e -> EngineVertex m v -> (Effect m) [(Result m) (EngineEdge m e)]

  -- | Get all edges terminating at a vertex, and satisying some
  --   characterization.
  getEdgesIn :: EngineEdgeInformation m e -> EngineVertex m v -> (Effect m) [(Result m) (EngineEdge m e)]

  -- | Insert a vertex into a graph.
  insertVertex :: EngineVertexInsertion m v -> (Effect m) ((Result m) (EngineVertex m v))

  -- | Insert an edge into a graph.
  insertEdge
    :: EngineEdgeInsertion m e
    -> EngineVertex m u
    -> EngineVertex m v
    -> (Effect m) (Result m (EngineEdge m e))
