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

  ) where

import Control.Monad.FInterpreter
import Control.Monad.Free
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

type GraphQuery g = Free (GraphQueryF g)

vertex :: Vertex g v => Proxy v -> EngineVertexInformation g v -> GraphQuery g (V g v)
vertex proxyV vinfo = liftF (GQVertex proxyV vinfo id)

edge :: Edge g e => Proxy e -> EngineEdgeInformation g e -> GraphQuery g (E g e)
edge proxyE einfo = liftF (GQEdge proxyE einfo id)

incoming
  :: Edge g e
  => Proxy e
  -> EngineEdgeInformation g e
  -> V g (EdgeTarget g e)
  -> GraphQuery g (E g e)
incoming proxyE einfo vert = liftF (GQIncoming proxyE einfo vert id)

outgoing
  :: Edge g e
  => Proxy e
  -> EngineEdgeInformation g e
  -> V g (EdgeSource g e)
  -> GraphQuery g (E g e)
outgoing proxyE einfo vert = liftF (GQOutgoing proxyE einfo vert id)

source :: Edge g e => E g e -> GraphQuery g (V g (EdgeSource g e))
source edg = liftF (GQSource edg id)

target :: Edge g e => E g e -> GraphQuery g (V g (EdgeTarget g e))
target edg = liftF (GQTarget edg id)

adjacentOut
  :: Edge g e
  => Proxy e
  -> EngineEdgeInformation g e
  -> V g (EdgeSource g e)
  -> GraphQuery g (V g (EdgeTarget g e))
adjacentOut proxyE einfo vert = outgoing proxyE einfo vert >>= target

adjacentIn
  :: Edge g e
  => Proxy e
  -> EngineEdgeInformation g e
  -> V g (EdgeTarget g e)
  -> GraphQuery g (V g (EdgeSource g e))
adjacentIn proxyE einfo vert = incoming proxyE einfo vert >>= source

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

type GraphMutation g = Free (GraphMutationF g)

putVertex
  :: Vertex g v
  => Proxy v
  -> EngineVertexInsertion g v
  -> GraphMutation g (V g v)
putVertex proxyV vinsert = liftF (GMVertex proxyV vinsert id)

putEdge
  :: Edge g e
  => Proxy e
  -> EngineEdgeInsertion g e
  -> V g (EdgeSource g e)
  -> V g (EdgeTarget g e)
  -> GraphMutation g (E g e)
putEdge proxyE einsert srcv tgtv = liftF (GMEdge proxyE einsert srcv tgtv id)

type GraphPatternsF g = GraphQueryF g :+: GraphMutationF g
type GraphPatterns g = Free (GraphPatternsF g)
