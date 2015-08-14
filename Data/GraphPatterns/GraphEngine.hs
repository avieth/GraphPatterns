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
import Data.Proxy

class
    ( Functor (GraphEngineMonad g)
    , Applicative (GraphEngineMonad g)
    , Monad (GraphEngineMonad g)
    ) => GraphEngine g
  where

    type GraphEngineMonad g :: * -> *

    -- | Type of vertices. Each member of this type must identify at most one
    --   vertex in an EngineGraph.
    type EngineVertex g v :: *

    -- | Type of edges. Each member of this type must identify at most one
    --   edge in an EngineGraph.
    type EngineEdge g e :: *

    -- | Vertex and Edge instances must provide injections into these types so
    --   that edges and vertices can be inserted into a graph. The GraphEngine
    --   will produce EngineVertex and EngineEdge instances from these via
    --   insertVertex, insertEdge.
    type EngineVertexInsertion g v :: *
    type EngineEdgeInsertion g e :: *

    -- | Type of (possibly partial) vertex information. Each member of this type
    --   must characterize 0 or more vertices in an EngineGraph.
    type EngineVertexInformation g v :: *
    type EngineEdgeInformation g e :: *

    -- | Get the terminal vertex of an edge.
    getTargetVertex
      :: Proxy g
      -> Proxy e
      -> Proxy v
      -> EngineEdge g e
      -> GraphEngineMonad g (EngineVertex g v)

    -- | Get the source vertex of an edge.
    getSourceVertex
      :: Proxy g
      -> Proxy e
      -> Proxy v
      -> EngineEdge g e
      -> GraphEngineMonad g (EngineVertex g v)

    -- | Get zero, one, or many @EngineVertex@s from relevant information.
    getVertices
      :: Proxy g
      -> Proxy v
      -> EngineVertexInformation g v
      -> GraphEngineMonad g (EngineVertex g v)

    -- | Get zero, one, or many @EngineEdge@s from relevant information.
    getEdges
      :: Proxy g
      -> Proxy e
      -> EngineEdgeInformation g e
      -> GraphEngineMonad g (EngineEdge g e)

    -- | Get all edges initiating from a vertex, and satisfying some
    --   characterization.
    getEdgesOut
      :: Proxy g
      -> Proxy e
      -> Proxy v
      -> EngineEdgeInformation g e
      -> EngineVertex g v
      -> GraphEngineMonad g (EngineEdge g e)

    -- | Get all edges terminating at a vertex, and satisying some
    --   characterization.
    getEdgesIn
      :: Proxy g
      -> Proxy e
      -> Proxy v
      -> EngineEdgeInformation g e
      -> EngineVertex g v
      -> GraphEngineMonad g (EngineEdge g e)

    -- | Insert a vertex into a graph.
    insertVertex
      :: Proxy g
      -> Proxy v
      -> EngineVertexInsertion g v
      -> GraphEngineMonad g (EngineVertex g v)

    -- | Insert an edge into a graph.
    insertEdge
      :: Proxy g
      -> Proxy e
      -> Proxy srcv
      -> Proxy tgtv
      -> EngineEdgeInsertion g e
      -> EngineVertex g srcv
      -> EngineVertex g tgtv
      -> GraphEngineMonad g (EngineEdge g e)

    -- | Alter the local properties of the EngineVertex so that it looks as
    --   though it were inserted using the EngineVertexInsertion, but maintains
    --   its adjacency.
    updateVertex
      :: Proxy g
      -> Proxy v
      -> EngineVertex g v
      -> EngineVertexInsertion g v
      -> GraphEngineMonad g (EngineVertex g v)

    updateEdge
      :: Proxy g
      -> Proxy e
      -> EngineEdge g e
      -> EngineEdgeInsertion g e
      -> GraphEngineMonad g (EngineEdge g e)

    -- | Must remove the vertex AND all of its incoming and outgoing edges.
    deleteVertex
      :: Proxy g
      -> Proxy v
      -> EngineVertex g v
      -> GraphEngineMonad g ()

    deleteEdge
      :: Proxy g
      -> Proxy e
      -> EngineEdge g e
      -> GraphEngineMonad g ()
