{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.GraphPatterns.Language (

    GraphPatterns
  , runGraphPatterns

  , vertex
  , edge
  , incoming
  , outgoing
  , source
  , target
  , adjacentOut
  , adjacentIn

  {-

    TODO hopIncoming takes n hops on incoming edges of a given type.
  , hopIncoming
    TODO hopOutgoing takes n hops on outgoing edges of a given type.
  , hopOutgoing
  -}

  ) where

import Data.GraphPatterns.GraphEngine
import Data.GraphPatterns.Anomaly
import Data.GraphPatterns.Vertex
import Data.GraphPatterns.Edge
import Data.GraphPatterns.Types

import Control.Applicative ((<$>), (<*>), Applicative, pure)
import Control.Monad (join)
import Data.Traversable (traverse, Traversable)
import Data.Proxy

-- | Our EDSL as a monad (for a fixed GraphEngine).
data GraphPatterns m a = GraphPatterns (m (Anomalized a))

unGraphPatterns :: GraphPatterns m a -> m (Anomalized a)
unGraphPatterns (GraphPatterns x) = x

-- | Given an EngineGraph we can run our GraphPatterns expression.
runGraphPatterns
  :: GraphEngine m
  => GraphPatterns m a
  -> EngineGraph m
  -> Anomalized a
runGraphPatterns (GraphPatterns x) g = runGraphEngine x g

instance Functor m => Functor (GraphPatterns m) where
  fmap f = GraphPatterns . ((fmap . fmap) f) . unGraphPatterns

instance Applicative m => Applicative (GraphPatterns m) where
  pure = GraphPatterns . pure . pure
  (<*>) f x = GraphPatterns $ (unGraphPatterns f) <*^*> (unGraphPatterns x)
    where (<*^*>) f x = (fmap (<*>) f) <*> x

instance Monad m => Monad (GraphPatterns m) where
  return = GraphPatterns . return . return
  -- TODO can we abstract this block like we did for <*> ?
  x >>= k = GraphPatterns $ do
      either <- unGraphPatterns x
      case either of
        Left y -> return $ Left y
        Right z -> unGraphPatterns $ k z

-- This is not the most general type.
vertex :: (DeterminesVertex m d v) => Proxy v -> d -> GraphPatterns m [v]
vertex vertexProxy determiner = GraphPatterns $ do
  let vertexInfo = toEngineVertexInformation Proxy vertexProxy determiner
  engineVertices <- getVertices vertexInfo

  -- Check for anomaly based on uniqueness...
  -- Do we even need type-level trickery for this? Can't we just ask the
  -- class to indicate unique or not unique at value level?
  --let anomalized = case engineVertices of
  --      [] -> Right []
  --      x -> Right x
  --

  -- If any of them are Nothing, we want an anomaly.
  -- That's to say, we want a function
  --
  --   Either Anomaly (Maybe a) -> Either Anomaly a
  --
  --   (a -> f b) -> t a -> f (t b)
  --
  -- Aha never mind, traverse takes care of this for us.
  --
  -- Hm, is there a good reason to not just use [] always, and do away with
  -- the One, Many types? We could just check this at the data level here.
  --
  --   data EdgeCardinality
  --     = OneToOne
  --     | ManyToMany
  --     | ManyToOne
  --     | OneToMany
  --
  --   data DeterminerUniqueness = Unique | NotUnique
  --
  -- and then have
  --
  --   (DeterminesVertex m d v) => Proxy m -> Proxy d -> Proxy v -> DeterminerUniqueness
  --   (Edge m e) => Proxy m -> Proxy e -> EdgeCardinality
  --
  -- Hm, yeah, either way we need type-level trickery, with all of those proxies.
  -- The alternative is to stick another clause in the type signature of this
  -- function vertex, saying that the output is
  --
  --   (ResultsWrapper t) => Anomalized (t v)
  --
  -- and we have
  --
  --   handleUniquenessAnomaly :: ResultWrapper t => Uniqueness -> [v] -> Anomalized (t v)
  --   handleUniquenessAnomaly True (x : y : _) = Left _
  --   handleUniquenessAnomaly True x = Right (One (Just x))
  --   handleUniquenessAnomaly False xs = Right (Many xs)
  --
  -- Yeah we can't implement that; the type system just won't allow it, even
  -- though One and Many are both ResultWrappers.
  --
  -- What we _really_ need are
  --
  --   vertexUniquenessAnomaly :: ResultWrapper t => [v] -> Anomalized (t v)
  --   edgeUniquenessAnomaly :: ResultWrapper t => [e] -> Anomalized (t e)
  --   edgeCardinalityAnomaly :: ResultWrapper t => [v] -> Anomalized (t v)
  --
  -- The uniqueness constraints take more, requires the type of the determiner
  -- and of course the graph engine.
  --
  --   vertexUniquenessAnomaly :: (DeterminesVertex m d v, ResultWrapper t)
  --     => Proxy m -> Proxy d -> [v] -> Anomalized (t v)
  --
  case traverse (fromEngineVertex Proxy) engineVertices of
    Nothing -> return $ Left undefined
    Just x -> return $ Right x

edge :: (DeterminesEdge m d e) => Proxy e -> d -> GraphPatterns m [e]
edge edgeProxy determiner = GraphPatterns $ do
  let edgeInfo = toEngineEdgeInformation Proxy edgeProxy determiner
  engineEdges <- getEdges edgeInfo
  case traverse (fromEngineEdge Proxy) engineEdges of
    Nothing -> return $ Left undefined
    Just x -> return $ Right x

incoming
  :: forall m e v d .
     ( Edge m e
     , Vertex m v
     , DeterminesLocalEdge m v e d
     , EdgeDirection m v e d ~ In
     )
  => d
  -> v
  -> GraphPatterns m [e]
incoming determiner v = GraphPatterns $ do
  let edgeInfo = toEngineEdgeInformationLocal Proxy (Proxy :: Proxy v) (Proxy :: Proxy e) determiner
  engineEdges <- getEdgesIn edgeInfo (toEngineVertex Proxy v)
  case traverse (fromEngineEdge Proxy) engineEdges of
    Nothing -> return $ Left undefined
    Just x -> return $ Right x

outgoing
  :: forall m e v d .
     ( Edge m e
     , Vertex m v
     , DeterminesLocalEdge m v e d
     , EdgeDirection m v e d ~ Out
     )
  => d
  -> v
  -> GraphPatterns m [e]
outgoing determiner v = GraphPatterns $ do
  let edgeInfo = toEngineEdgeInformationLocal Proxy (Proxy :: Proxy v) (Proxy :: Proxy e) determiner
  engineEdges <- getEdgesIn edgeInfo (toEngineVertex Proxy v)
  case traverse (fromEngineEdge Proxy) engineEdges of
    Nothing -> return $ Left undefined
    Just x -> return $ Right x

source
  :: forall m e .
     ( Edge m e
     )
  => e
  -> GraphPatterns m (EdgeSource m e)
source edge = GraphPatterns $ do
  let engineEdge = toEngineEdge Proxy edge
  engineSourceVertex <- getSourceVertex engineEdge
  case fromEngineVertex (Proxy :: Proxy m) engineSourceVertex of
    Nothing -> return $ Left undefined
    Just x -> return $ Right x

target
  :: forall m e .
     ( Edge m e
     )
  => e
  -> GraphPatterns m (EdgeTarget m e)
target edge = GraphPatterns $ do
  let engineEdge = toEngineEdge Proxy edge
  engineTargetVertex <- getTargetVertex engineEdge
  case fromEngineVertex (Proxy :: Proxy m) engineTargetVertex of
    Nothing -> return $ Left undefined
    Just x -> return $ Right x

adjacentOut
  :: forall m e d .
     ( DeterminesLocalEdge m (EdgeSource m e) e d
     , EdgeDirection m (EdgeSource m e) e d ~ Out
     )
  => Proxy e
  -- ^ Somehow, we need the proxy to avoid ambiguity, but we never actually
  -- use the relevant value or its type... do we?
  -- Can't wrap my head around this witchcraft.
  -> d
  -> EdgeSource m e
  -> GraphPatterns m [EdgeTarget m e]
adjacentOut proxy d v = do
  outs :: [e] <- outgoing d v
  -- ^ This type annotation is essential; without it we get ambiguity!
  mapM target outs

adjacentIn
  :: forall m e d .
     ( DeterminesLocalEdge m (EdgeTarget m e) e d
     , EdgeDirection m (EdgeTarget m e) e d ~ In
     )
  => Proxy e
  -> d
  -> EdgeTarget m e
  -> GraphPatterns m [EdgeSource m e]
adjacentIn proxy d v = do
  ins :: [e] <- incoming d v
  mapM source ins
