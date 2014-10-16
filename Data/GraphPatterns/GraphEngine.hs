{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ExistentialQuantification #-}

module Data.GraphPatterns.GraphEngine (
    GraphEngine(..)
  , ManyToOne
  , OneToMany
  , ManyToMany
  , OneToOne
  , EdgeLabel(..)
  , Incoming(..)
  , Outgoing(..)
  , Both(..)
  , EdgeTraversalResult
  , Many
  , One
  , Anomaly(..)
  , HandlesAnomaly(..)
  ) where

import Control.Applicative (Applicative)
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
  toEngine :: l -> EngineEdgeLabel m
  showEdgeLabel :: l -> String
  showEdgeLabel _ = "UnknownEdgeLabel"

data Incoming = Incoming
data Outgoing = Outgoing
data Both = Both

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

-- | We choose existential types because universal quantification would demand
--   that edge traversal datatypes grow to include a new Either Anomaly for 
--   every hop; we wouldn't be able to collapse anomalies for different edge
--   labels into one anomaly. With existential types we can.
--
--   Also, TBD are the (GraphEngine m, v ~ Vertex m) constraints really needed?
data Anomaly = forall l v m . (GraphEngine m, v ~ Vertex m, EdgeLabel l) => EdgeCardinalityAnomaly l v

instance Show Anomaly where
  show (EdgeCardinalityAnomaly l v) = showEdgeLabel l

-- | This class allows us to dispatch anomalies through more than one type, i.e.
--   through Many and One. Instances of this class are where we code the
--   conditions under which an anomaly occurs (the only case is when a One is
--   to had but there is more than one edge).
class HandlesAnomaly a where
  handleAnomaly
    :: ( EdgeLabel l
       , GraphEngine m
       , EdgeTraversalResult d (EdgeCardinality l) b ~ a b
       )
    => d
    -> l
    -> Vertex m
    -> [b]
    -> Either Anomaly (a b)

instance HandlesAnomaly One where
  handleAnomaly _ _ _ [] = Right $ One Nothing
  handleAnomaly _ _ _ [x] = Right $ One (Just x)
  handleAnomaly _ l v _ = Left $ EdgeCardinalityAnomaly l v

instance HandlesAnomaly Many where
  handleAnomaly _ _ _ xs = Right $ Many xs

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
  data EngineEdgeLabel m :: *

  -- | Type of vertices.
  data Vertex m :: *

  -- | Type of edges.
  data Edge m :: *

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
    :: ( HandlesAnomaly a
       , EdgeLabel l
       , EdgeTraversalResult Outgoing (EdgeCardinality l) (Edge m) ~ a (Edge m))
    => l
    -- ^ The intention is that this guy is a value used only for its type a la
    -- singleton types... call it a singleton value?
    -> Vertex m
    -> m (Either Anomaly (a (Edge m)))

  -- | Given a Vertex, produce a list of all Edges incoming, i.e. with tail
  --   ends at the Vertex.
  getEdgesIn
    :: ( HandlesAnomaly a
       , EdgeLabel l
       , EdgeTraversalResult Incoming (EdgeCardinality l) (Edge m) ~ a (Edge m))
    => l
    -- ^ The intention is that this guy is a value used only for its type a la
    -- singleton types... call it a singleton value?
    -> Vertex m
    -> m (Either Anomaly (a (Edge m)))



  -- | Get the vertex to which an edge goes in (head of edge is here).
  getTargetVertex :: Edge m -> m (Vertex m)

  -- | Get the vertex from which an edge goes out (tail of edge is here).
  getSourceVertex :: Edge m -> m (Vertex m)
