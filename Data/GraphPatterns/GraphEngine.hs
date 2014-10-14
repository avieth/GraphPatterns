{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.GraphPatterns.GraphEngine (
    GraphEngine(..)
  , ManyToOne
  , OneToMany
  , ManyToMany
  , OneToOne
  , EdgeLabel(..)
  , Incoming
  , Outgoing
  , Both
  , EdgeTraversalResult
  , Many
  , One
  ) where

import Control.Applicative (Applicative)
import Data.Functor.Identity (Identity, runIdentity)
import Data.Traversable (Traversable)
import Data.Foldable (Foldable)

data ManyToOne
data OneToMany
data ManyToMany
data OneToOne

class EdgeLabel l where
  -- | Must be one of ManyToOne, OneToMany, ManyToMany, OneToOne
  --   Can we enforce this?
  type EdgeCardinality l :: *
  toEngine :: GraphEngine m => l -> EngineEdgeLabel m

data Incoming
data Outgoing
data Both

-- | Type to represent many of some thing. Naturally, we choose the list.
newtype Many a = Many [a]
  deriving (Functor, Applicative, Monad, Foldable, Traversable)

-- | Type to represent at most one of some thing. Naturally, we choose maybe.
newtype One a = One (Maybe a)
  deriving (Functor, Applicative, Monad, Foldable, Traversable)

-- Image of this type family is the type to use for
--   getEdgesOut if direction ~ Outgoing
--   getEdgesIn if direction ~ Incoming
-- and the edge label l has EdgeCardinality l ~ cardinality.
-- TODO should be closed type family but I don't have GHC 7.8.1
--
-- Need to tell GHC that every outcome of this family is a Traversable. How to
-- do that?
--
--   class EdgeTraversalResult direction cardinality a where
--     type 
--     
type family EdgeTraversalResult direction cardinality a
type instance EdgeTraversalResult Incoming ManyToOne a = Many a
type instance EdgeTraversalResult Incoming ManyToMany a = Many a
type instance EdgeTraversalResult Incoming OneToMany a = One a
type instance EdgeTraversalResult Incoming OneToOne a = One a
type instance EdgeTraversalResult Outgoing ManyToOne a = One a
type instance EdgeTraversalResult Outgoing ManyToMany a = Many a
type instance EdgeTraversalResult Outgoing OneToMany a = Many a
type instance EdgeTraversalResult Outgoing OneToOne a = One a
type instance EdgeTraversalResult Both ManyToOne a = Many a
type instance EdgeTraversalResult Both ManyToMany a = Many a
type instance EdgeTraversalResult Both OneToMany a = Many a
type instance EdgeTraversalResult Both OneToOne a = One a

-- | Class to describe anything that can be used as a graph engine.
--   We don't want to force it to be in IO, because pure Haskell graph
--   engines should work as well. But, we do demand functor, applicative, monad
--   because without these, it simply wouldn't work. For pure graph engines
--   like graph-core, all you've got to do is wrap them in an appropriate
--   datatype.
class (Functor m, Applicative m, Monad m) => GraphEngine m where

  -- | Type of the thing from which we pull our info. Needed only for
  --   runGraphEngine.
  type Graph m :: *

  -- | Type of vertex identifiers.
  type VertexId m :: *

  -- | Type of edge identifiers.
  --   If your graph doesn't have edge identifiers, you can use the Void type
  --   to make sure that nobody every calls getEdgeById.
  type EdgeId m :: *

  -- | Type of edge labels. If a given implementation of Graph does not support
  --   this notion, you should use the type ().
  type EngineEdgeLabel m :: *

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
  --   Cannot give a default implementation for undirected graphs because we
  --   have the Outgoing type in there.
  getEdgesOut
    :: (EdgeLabel l)
    => l
    -- ^ The intention is that this guy is a value used only for its type a la
    -- singleton types... call it a singleton value?
    -> Vertex m
    -> m (EdgeTraversalResult Outgoing (EdgeCardinality l) (Edge m))

  -- | Given a Vertex, produce a list of all Edges incoming, i.e. with tail
  --   ends at the Vertex.
  getEdgesIn
    :: (EdgeLabel l)
    => l
    -> Vertex m
    -> m (EdgeTraversalResult Incoming (EdgeCardinality l) (Edge m))

  -- | Get the vertex to which an edge goes in (head of edge is here).
  getTargetVertex :: Edge m -> m (Vertex m)

  -- | Get the vertex from which an edge goes out (tail of edge is here).
  getSourceVertex :: Edge m -> m (Vertex m)
