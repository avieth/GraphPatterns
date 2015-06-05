{-|
Module      : Data.GraphPatterns.Language
Description : Definition of the GraphPatterns language.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.GraphPatterns.Language (

    GraphQueryF(..)
  , GraphQuery
  , GraphMutationF(..)
  , GraphMutation
  , GraphPatternsF
  , GraphPatterns
  , V(..)
  , E(..)
  , v
  , e
  , engineV
  , engineE

  , vertex
  , edge
  , incoming
  , outgoing
  , source
  , target
  , adjacentIn
  , adjacentOut

  , putVertex
  , putEdge

  , query
  , mutation

  ) where

import Control.Monad.FInterpreter
import Control.Monad.Trans.Free
import Data.Proxy
import Data.GraphPatterns.Vertex
import Data.GraphPatterns.Edge
import Data.GraphPatterns.GraphEngine

-- | This type carries an EngineVertex and a Vertex. It shall be produced
--   by our DSL primitives. It is here so that we don't have to continually
--   convert to and from EngineVertex.
data V g v where
    V :: EngineVertex g v -> v -> V g v

-- | Like V, this type is here to prevent superfluous conversions from
--   engine types.
data E g e where
    E :: EngineEdge g e -> e -> E g e

instance Show v => Show (V g v) where
  show (V _ v) = "V " ++ show v

instance Show e => Show (E g e) where
  show (E _ e) = "E " ++ show e

v :: V g v -> v
v (V _ v) = v

e :: E g e -> e
e (E _ e) = e

-- | Projection from V onto its EngineVertex.
--   Requires a proxy in order to determine which EngineVertex instance to use.
engineV :: V g v -> EngineVertex g v
engineV (V ev _) = ev

-- | Projection from E onto its EngineEdge.
--   Requires a proxy in order to determine which EngineEdge instance to use.
engineE :: E g e -> EngineEdge g e
engineE (E ee _) = ee

data GraphQueryF g t where

    GQVertex
      :: (Vertex g v)
      => Proxy v
      -> EngineVertexInformation g v
      -> (V g v -> t)
      -> GraphQueryF g t

    GQEdge
      :: (Edge g e)
      => Proxy e
      -> EngineEdgeInformation g e
      -> (E g e -> t)
      -> GraphQueryF g t

    GQIncoming
      :: (Edge g e)
      => Proxy e
      -> EngineEdgeInformation g e
      -> V g (EdgeTarget g e)
      -> (E g e -> t)
      -> GraphQueryF g t

    GQOutgoing
      :: (Edge g e)
      => Proxy e
      -> EngineEdgeInformation g e
      -> V g (EdgeSource g e)
      -> (E g e -> t)
      -> GraphQueryF g t

    GQSource
      :: (Edge g e)
      => E g e
      -> (V g (EdgeSource g e) -> t)
      -> GraphQueryF g t

    GQTarget
      :: (Edge g e)
      => E g e
      -> (V g (EdgeTarget g e) -> t)
      -> GraphQueryF g t

instance Functor (GraphQueryF g) where
    fmap f term = case term of
        GQVertex proxy vinfo next -> GQVertex proxy vinfo (fmap f next)
        GQEdge proxy einfo next -> GQEdge proxy einfo (fmap f next)
        GQIncoming proxy einfo vert next -> GQIncoming proxy einfo vert (fmap f next)
        GQOutgoing proxy einfo vert next -> GQOutgoing proxy einfo vert (fmap f next)
        GQSource edg next -> GQSource edg (fmap f next)
        GQTarget edg next -> GQTarget edg (fmap f next)

type GraphQuery g = FreeT (GraphQueryF g)

vertex
  :: forall g v d m .
     ( DeterminesVertex g v d
     , Monad m
     )
  => Proxy v
  -> d
  -> GraphQuery g m (V g v)
vertex proxyV determiner = liftF (GQVertex proxyV vinfo id)
  where
    vinfo = toEngineVertexInformation (Proxy :: Proxy g) proxyV determiner

edge
  :: forall g e d m .
     ( DeterminesEdge g e d
     , Monad m
     )
  => Proxy e
  -> d
  -> GraphQuery g m (E g e)
edge proxyE determiner = liftF (GQEdge proxyE einfo id)
  where
    einfo = toEngineEdgeInformation (Proxy :: Proxy g) proxyE determiner

incoming
  :: forall g e d m .
     ( DeterminesLocalEdge g e d
     , Monad m
     )
  => Proxy e
  -> d
  -> V g (EdgeTarget g e)
  -> GraphQuery g m (E g e)
incoming proxyE determiner vert = liftF (GQIncoming proxyE einfo vert id)
  where
    einfo = toEngineEdgeInformationLocal (Proxy :: Proxy g) proxyE determiner

outgoing
  :: forall g e d m .
     ( DeterminesLocalEdge g e d
     , Monad m
     )
  => Proxy e
  -> d
  -> V g (EdgeSource g e)
  -> GraphQuery g m (E g e)
outgoing proxyE determiner vert = liftF (GQOutgoing proxyE einfo vert id)
  where
    einfo = toEngineEdgeInformationLocal (Proxy :: Proxy g) proxyE determiner

source
  :: ( Edge g e
     , Monad m
     )
  => E g e
  -> GraphQuery g m (V g (EdgeSource g e))
source edg = liftF (GQSource edg id)

target
  :: ( Edge g e
     , Monad m
     )
  => E g e
  -> GraphQuery g m (V g (EdgeTarget g e))
target edg = liftF (GQTarget edg id)

adjacentOut
  :: ( DeterminesLocalEdge g e d
     , Monad m
     )
  => Proxy e
  -> d
  -> V g (EdgeSource g e)
  -> GraphQuery g m (V g (EdgeTarget g e))
adjacentOut proxyE determiner vert = outgoing proxyE determiner vert >>= target

adjacentIn
  :: ( DeterminesLocalEdge g e d
     , Monad m
     )
  => Proxy e
  -> d
  -> V g (EdgeTarget g e)
  -> GraphQuery g m (V g (EdgeSource g e))
adjacentIn proxyE determiner vert = incoming proxyE determiner vert >>= source

data GraphMutationF g t where

    GMVertex
      :: (Vertex g v)
      => Proxy v
      -> EngineVertexInsertion g v
      -> (V g v -> t)
      -> GraphMutationF g t

    GMEdge
      :: (Edge g e)
      => Proxy e
      -> EngineEdgeInsertion g e
      -> V g (EdgeSource g e)
      -> V g (EdgeTarget g e)
      -> (E g e -> t)
      -> GraphMutationF g t

instance Functor (GraphMutationF g) where
    fmap f term = case term of
        GMVertex proxyV vi next -> GMVertex proxyV vi (fmap f next)
        GMEdge proxyE ei srcv tgtv next -> GMEdge proxyE ei srcv tgtv (fmap f next)

type GraphMutation g = FreeT (GraphMutationF g)

putVertex
  :: forall g v m .
     ( Vertex g v
     , Monad m
     )
  => v
  -> GraphMutation g m (V g v)
putVertex v = liftF (GMVertex proxyV vinsert id)
  where
    proxyV :: Proxy v
    proxyV = Proxy
    proxyG :: Proxy g
    proxyG = Proxy
    vinsert :: EngineVertexInsertion g v
    vinsert = toEngineVertexInsertion proxyG v

putEdge
  :: forall g e m .
     ( Edge g e
     , Monad m
     )
  => e
  -> V g (EdgeSource g e)
  -> V g (EdgeTarget g e)
  -> GraphMutation g m (E g e)
putEdge edg srcv tgtv = liftF (GMEdge proxyE einsert srcv tgtv id)
  where
    proxyE :: Proxy e
    proxyE = Proxy
    proxyG :: Proxy g
    proxyG = Proxy
    einsert :: EngineEdgeInsertion g e
    einsert = toEngineEdgeInsertion proxyG edg

type GraphPatternsF g = GraphQueryF g :+: GraphMutationF g
type GraphPatterns g = FreeT (GraphPatternsF g)

query :: Monad m => GraphQuery g m t -> GraphPatterns g m t
query = injectF

mutation :: Monad m => GraphMutation g m t -> GraphPatterns g m t
mutation = injectF
