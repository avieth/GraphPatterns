{-|
Module      : Data.GraphPatterns.Language
Description : Definition of the GraphPatterns language.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.GraphPatterns.Language (

    GraphQueries
  , runGraphQueries
  , QueryResult
  , runQueryResult

  , V
  , E
  , v
  , e

  , vertex
  , edge

  , incoming
  , outgoing
  , source
  , target
  , adjacentOut
  , adjacentIn

  --  TODO hopIncoming takes n hops on incoming edges of a given type.
  --, hopIncoming
  --  TODO hopOutgoing takes n hops on outgoing edges of a given type.
  --, hopOutgoing

  , GraphMutations
  , runGraphMutations
  , putVertex
  , putEdge

  , GraphPatterns
  , runGraphPatterns
  , runGraphPatterns'
  , query 
  -- ^ injects a GraphQueries into GraphPatterns
  , mutation
  -- ^ injects a GraphMutations into GraphPatterns

  ) where

import Prelude hiding (concat)

import Data.GraphPatterns.MList
import Data.GraphPatterns.GraphEngine
import Data.GraphPatterns.Anomaly
import Data.GraphPatterns.Vertex
import Data.GraphPatterns.Edge
import Data.GraphPatterns.Types

import Data.TypeRelation

import Control.Applicative
import Control.Monad
import Control.Monad.TransT
import Control.Monad.Trans.Class (lift)

import Data.Traversable (traverse, Traversable)
import Data.Foldable
import Data.Proxy

-- TODO We need more structure here: a tranformer over MList determined by
-- the graph engine (like m). We must be able to separate the outermost monad
-- m (probably an IO-capable thing) from the innermost monad f (almost certainly
-- not side-effecting).
newtype QueryResultT m f a = QueryResultT {
    runQueryResultT :: TransT (MList m) f a
  } deriving (Functor, Applicative, Monad, Alternative, MonadPlus)

-- For exporting.
type QueryResult = QueryResultT
runQueryResult = runQueryResultT

-- Lift some monadic value into QueryResult.
liftQ :: (Functor m, Monad m) => m (f a) -> QueryResultT m f a
liftQ = liftListQ . fmap pure

liftListQ :: (Functor m, Monad m) => m [f a] -> QueryResultT m f a
liftListQ = QueryResultT . TransT . ml_fromlist'

-- | Our query EDSL is just a new name for the QueryResult monad, over some
--   other monad, which in practice shall be a GraphEngine.
newtype GraphQueries m a = GraphQueries {
    runGraphQueries :: QueryResultT (Effect m) (Result m) a
  }

deriving instance GraphEngine m => Functor (GraphQueries m)
deriving instance GraphEngine m => Applicative (GraphQueries m)
deriving instance GraphEngine m => Monad (GraphQueries m)
deriving instance GraphEngine m => Alternative (GraphQueries m)
deriving instance GraphEngine m => MonadPlus (GraphQueries m)

-- | This type carries an EngineVertex and a Vertex. It shall be produced
--   by our DSL primitives. It is here so that we don't have to continually
--   convert to and from EngineVertex.
data V ev v = V ev v

-- | Like V, this type is here to prevent superfluous conversions from
--   engine types.
data E ee e = E ee e

instance Show v => Show (V ev v) where
  show (V _ v) = "V " ++ show v

instance Show e => Show (E ee e) where
  show (E _ e) = "E " ++ show e

-- | Projection from V onto its EngineVertex.
--   Requires a proxy in order to determine which EngineVertex instance to use.
engineV
  :: forall m v ev u .
     ( ev ~ EngineVertex m v
     )
  => u m
  -> V ev v
  -> ev
engineV _ (V ev _) = ev

-- | Projection from E onto its EngineEdge.
--   Requires a proxy in order to determine which EngineEdge instance to use.
engineE
  :: forall m e ee u .
     ( ee ~ EngineEdge m e
     )
  => u m
  -> E ee e
  -> ee
engineE _ (E ee _) = ee

-- | Projection from V onto something of which the Vertex is a smaller type.
v
  :: forall m v w u .
     ( Smaller v w
     )
  => u w
  -> V m v
  -> w
v _ (V _ x) = inject x

-- | Projection from E onto something of which the Edge is a smaller type.
e
  :: forall m e f u .
     ( Smaller e f
     )
  => u f
  -> E m e
  -> f
e _ (E _ x) = inject x

-- | Injection into V; this can only be done within the GraphQueries
--   monad, because an EngineVertex must also be defined!
--
--   NB there can be no injection of a Vertex m v into V m v ! That's
--   because a Vertex m v is just not enough information to construct an
--   EngineVertex m v.
v'
  :: forall m v ev .
     ( EngineVertex m v ~ ev
     , Vertex m v
     , CommuteM (Result m)
     , Functor (Effect m)
     , Applicative (Effect m)
     , Monad (Effect m)
     , Functor (Result m)
     , Applicative (Result m)
     , Monad (Result m)
     )
  => ev
  -> GraphQueries m (V ev v)
v' engineVertex = GraphQueries $ do
    vert :: v <- liftQ $ fromEngineVertex engineVertex
    return $ V engineVertex vert

-- Injection into E; this can only be done within the GraphQueries
-- monad, because an EngineEdge must also be defined!
e'
  :: forall m e ee .
     ( EngineEdge m e ~ ee
     , Edge m e
     , CommuteM (Result m)
     , Functor (Effect m)
     , Applicative (Effect m)
     , Monad (Effect m)
     , Functor (Result m)
     , Applicative (Result m)
     , Monad (Result m)
     )
  => ee
  -> GraphQueries m (E ee e)
e' engineEdge = GraphQueries $ do
    edg :: e <- liftQ $ fromEngineEdge engineEdge
    return $ E engineEdge edg

-- | Ask for a Vertex, subject to a given determiner.
vertex
  :: forall m d v ev u .
     ( DeterminesVertex m v d
     , ev ~ EngineVertex m v
     , CommuteM (Result m)
     , Functor (Effect m)
     , Applicative (Effect m)
     , Monad (Effect m)
     , Functor (Result m)
     , Applicative (Result m)
     , Monad (Result m)
     )
  => u v
  -> d
  -> GraphQueries m (V ev v)
vertex _ determiner = GraphQueries $ do
  let vertexInfo :: EngineVertexInformation m v = toEngineVertexInformation determiner
  engineVertex <- liftListQ $ getVertices vertexInfo
  runGraphQueries $ v' engineVertex

-- | Ask for an Edge, subject to a given determiner.
edge
  :: forall m d e ee u .
     ( DeterminesEdge m e d
     , ee ~ EngineEdge m e
     , CommuteM (Result m)
     , Functor (Effect m)
     , Applicative (Effect m)
     , Monad (Effect m)
     , Functor (Result m)
     , Applicative (Result m)
     , Monad (Result m)
     )
  => u e
  -> d
  -> GraphQueries m (E ee e)
edge _ determiner = GraphQueries $ do
  let edgeInfo :: EngineEdgeInformation m e = toEngineEdgeInformation determiner
  engineEdge <- liftListQ $ getEdges edgeInfo
  runGraphQueries $ e' engineEdge

-- | Ask for all edges incoming to some Vertex, subject to a particular
--   determiner.
incoming
  :: forall m e s t d ee et u u' .
     ( Edge m e
     , Vertex m t
     , Vertex m s
     , DeterminesLocalEdge m e s t d
     , EdgeRelated e s t
     , et ~ EngineVertex m t
     , ee ~ EngineEdge m e
     , CommuteM (Result m)
     , Functor (Effect m)
     , Applicative (Effect m)
     , Monad (Effect m)
     , Functor (Result m)
     , Applicative (Result m)
     , Monad (Result m)
     )
  => u e
  -> u' s
  -- ^ Must disambiguate the source vertex.
  -> d
  -- ^ A determiner
  -> V et t
  -- ^ A V with the target vertex and its engine vertex representation.
  -> GraphQueries m (E ee e)
incoming _ proxyS determiner vertex = GraphQueries $ do
  let edgeInfo :: EngineEdgeInformation m e = toEngineEdgeInformationLocal proxyS (Proxy :: Proxy t) determiner
  engineEdge <- liftListQ $ getEdgesIn edgeInfo (engineV (Proxy :: Proxy m) vertex)
  runGraphQueries $ e' engineEdge

-- | Ask for all edges outgoing from some Vertex, subject to a particular
--   determiner.
outgoing
  :: forall m e v s t d ee es u u' .
     ( Edge m e
     , Vertex m s
     , Vertex m t
     , DeterminesLocalEdge m e s t d
     , EdgeRelated e s t
     , es ~ EngineVertex m s
     , ee ~ EngineEdge m e
     , CommuteM (Result m)
     , Functor (Effect m)
     , Applicative (Effect m)
     , Monad (Effect m)
     , Functor (Result m)
     , Applicative (Result m)
     , Monad (Result m)
     )
  => u e
  -> u' t
  -> d
  -> V es s
  -> GraphQueries m (E ee e)
outgoing _ proxyT determiner vertex = GraphQueries $ do
  let edgeInfo = toEngineEdgeInformationLocal (Proxy :: Proxy s) proxyT determiner
  engineEdge <- liftListQ $ getEdgesOut edgeInfo (engineV (Proxy :: Proxy m) vertex)
  runGraphQueries $ e' engineEdge

-- | Ask for the source of an edge, i.e. the Vertex out of which is extends.
source
  :: forall m e s t ee es u u' .
     ( Edge m e
     , Vertex m s
     , EdgeRelated e s t
     , ee ~ EngineEdge m e
     , es ~ EngineVertex m s
     , CommuteM (Result m)
     , Functor (Effect m)
     , Applicative (Effect m)
     , Monad (Effect m)
     , Functor (Result m)
     , Applicative (Result m)
     , Monad (Result m)
     )
  => u s
  -> u' t
  -- ^ We use a proxy to disambiguate the EdgeRelated instance.
  -> E ee e
  -> GraphQueries m (V es s)
source _ _ edge = GraphQueries $ do
  sourceVertex <- liftQ $ getSourceVertex (engineE (Proxy :: Proxy m) edge)
  runGraphQueries $ v' sourceVertex

target
  :: forall m e s t ee et u u' .
     ( Edge m e
     , Vertex m t
     , EdgeRelated e s t
     , ee ~ EngineEdge m e
     , et ~ EngineVertex m t
     , CommuteM (Result m)
     , Functor (Effect m)
     , Applicative (Effect m)
     , Monad (Effect m)
     , Functor (Result m)
     , Applicative (Result m)
     , Monad (Result m)
     )
  => u s
  -> u' t
  -> E ee e
  -> GraphQueries m (V et t)
target _ _ edge = GraphQueries $ do
  targetVertex <- liftQ $ getTargetVertex (engineE (Proxy :: Proxy m) edge)
  runGraphQueries $ v' targetVertex

-- | Get the Vertex across an outgoing edge, subject to an edge determiner.
adjacentOut
  :: forall m d e s t es et ee u u' .
     ( DeterminesLocalEdge m e s t d
     , EdgeRelated e s t
     , Vertex m s
     , Vertex m t
     , es ~ EngineVertex m s
     , et ~ EngineVertex m t
     , ee ~ EngineEdge m e
     , CommuteM (Result m)
     , Functor (Effect m)
     , Applicative (Effect m)
     , Monad (Effect m)
     , Functor (Result m)
     , Applicative (Result m)
     , Monad (Result m)
     )
  => u e
  -> u' t
  -> d
  -> V es s
  -> GraphQueries m (V et t)
adjacentOut proxyE proxyT d vertex = do
  outg :: E ee e <- outgoing proxyE proxyT d vertex
  -- ^ This type annotation is essential; without it we get ambiguity!
  --target $ E (Left outg)
  target (Proxy :: Proxy s) proxyT outg

-- | Get the Vertex across an incoming edge, subject to an edge determiner.
adjacentIn
  :: forall m d e s t es et ee u u' .
     ( DeterminesLocalEdge m e s t d
     , EdgeRelated e s t
     , Vertex m s
     , Vertex m t
     , es ~ EngineVertex m s
     , et ~ EngineVertex m t
     , ee ~ EngineEdge m e
     , CommuteM (Result m)
     , Functor (Effect m)
     , Applicative (Effect m)
     , Monad (Effect m)
     , Functor (Result m)
     , Applicative (Result m)
     , Monad (Result m)
     )
  => u e
  -> u' s
  -> d
  -> V et t
  -> GraphQueries m (V es s)
adjacentIn proxyE proxyS d vertex = do
  inc :: E ee e <- incoming proxyE proxyS d vertex
  source proxyS (Proxy :: Proxy t) inc

newtype GraphMutations m a = GraphMutations {
    runGraphMutations :: TransT (Effect m) (Result m) a
  }

deriving instance GraphEngine m => Functor (GraphMutations m)
deriving instance GraphEngine m => Applicative (GraphMutations m)
deriving instance GraphEngine m => Monad (GraphMutations m)

-- | Given any Vertex, we can mutate the graph by adding a Vertex of any
--   type such that the former Vertex is a SubVertex of the latter.
--   You give the SubVertex, you get back and EngineVertex of the super
--   vertex, resting assured that queries on the super vertex will work
--   as expected.
putVertex
  :: forall m v ev .
     ( Vertex m v
     , ev ~ EngineVertex m v
     , CommuteM (Result m)
     , Functor (Effect m)
     , Applicative (Effect m)
     , Monad (Effect m)
     , Functor (Result m)
     , Applicative (Result m)
     , Monad (Result m)
     )
  => v
  -> GraphMutations m (V ev v)
putVertex v = GraphMutations $ do
  let engineVertex :: EngineVertexInsertion m v = toEngineVertexInsertion v
  engineVertex <- TransT $ insertVertex engineVertex
  return $ V engineVertex v

putEdge
  :: forall m e s t es et ee .
     ( Edge m e
     , Vertex m s
     , Vertex m t
     , es ~ EngineVertex m s
     , et ~ EngineVertex m t
     , ee ~ EngineEdge m e
     , EdgeRelated e s t
     , CommuteM (Result m)
     , Functor (Effect m)
     , Applicative (Effect m)
     , Monad (Effect m)
     , Functor (Result m)
     , Applicative (Result m)
     , Monad (Result m)
     )
  => e
  -> V es s
  -> V et t
  -> GraphMutations m (E ee e)
putEdge e u v = GraphMutations $ do
  -- TODO check edge cardinality constraints and do not insert if it's violated.
  -- That will require doing a query before a mutation.
  -- TBD Is it possible to offload this to the GraphEngine if it supports it?
  let edgeInsertion :: EngineEdgeInsertion m e = toEngineEdgeInsertion e
      engineSource = engineV (Proxy :: Proxy m) u
      engineTarget = engineV (Proxy :: Proxy m) v
  engineEdge <- TransT $ insertEdge edgeInsertion engineSource engineTarget
  return $ E engineEdge e

-- | The all-powerful GraphPatterns monad is just GraphQueries with injectors
--   for GraphQueries and GraphMutations.
--   NB your GraphMutations happen in each branch of the query!
newtype GraphPatterns m a = GraphPatterns {
    unGraphPatterns :: GraphQueries m a
  } deriving (Functor, Applicative, Monad, Alternative, MonadPlus)

type GraphPatternsResult = QueryResultT

query :: GraphQueries m a -> GraphPatterns m a
query = GraphPatterns

mutation
  :: GraphEngine m
  => GraphMutations m a
  -> GraphPatterns m a
mutation = GraphPatterns . GraphQueries . liftQ . runTransT . runGraphMutations

runGraphPatterns :: GraphPatterns m a -> GraphPatternsResult (Effect m) (Result m) a
runGraphPatterns = runGraphQueries . unGraphPatterns

runGraphPatterns' :: (GraphEngine m) => GraphPatterns m a -> (Effect m) [(Result m) a]
runGraphPatterns' = ml_tolist . runTransT . runQueryResult . runGraphPatterns
