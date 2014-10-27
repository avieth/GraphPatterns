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

  , GraphMutations
  , runGraphMutations
  , putVertex
  , putEdge

  ) where

import Prelude hiding (concat)
import Data.GraphPatterns.GraphEngine
import Data.GraphPatterns.Anomaly
import Data.GraphPatterns.Vertex
import Data.GraphPatterns.Edge
import Data.GraphPatterns.Types

import Control.Applicative
import Control.Monad
import Data.Traversable (traverse, Traversable)
import Data.Foldable
import Data.Proxy

-- | We begin by defining the GraphPatterns monad for querying a graph

(<*^*>) f x = (fmap (<*>) f) <*> x
(<|^|>) x y = (fmap (<|>) x) <*> y

newtype Anomalized a = Anomalized {
    unAnomalize :: Either Anomaly a
  } deriving (Functor, Applicative, Monad, Foldable, Traversable, Show)

anomaly :: Anomaly -> Anomalized a
anomaly = Anomalized . Left

noAnomaly :: a -> Anomalized a
noAnomaly = Anomalized . Right

newtype GPResult a = GPResult {
    _unGPResult :: Anomalized [a]
  } deriving (Functor, Foldable, Traversable)

result :: Anomalized [a] -> GPResult a
result = GPResult

instance Applicative GPResult where
  pure = GPResult . pure . pure
  f <*> x = GPResult $ (_unGPResult f) <*^*> (_unGPResult x)

instance Alternative GPResult where
  empty = GPResult . pure $ empty
  x <|> y = GPResult $ (_unGPResult x) <|^|> (_unGPResult y)

instance Monad GPResult where
  return = GPResult . return . return
  x >>= k = GPResult $ do
    ys <- _unGPResult x
    -- ^ ys :: [a]
    zs <- traverse (_unGPResult . k) ys
    -- ^ za :: [[a]]
    return $ concat zs
    -- This monad definition is probably not what we want, but I think it'll
    -- work for now.

instance MonadPlus GPResult where
  mzero = GPResult . return $ mzero
  x `mplus` y = GPResult $ do
    x' <- _unGPResult x
    y' <- _unGPResult y
    return $ x' `mplus` y'

-- | Our EDSL as a monad (for a fixed GraphEngine).
data GraphPatterns m a = GraphPatterns (m (GPResult a))

unGraphPatterns :: GraphPatterns m a -> m (GPResult a)
unGraphPatterns (GraphPatterns x) = x

-- | Given an EngineGraph we can run our GraphPatterns expression.
runGraphPatterns
  :: GraphEngine m
  => GraphPatterns m a
  -> EngineGraph m
  -> GPResult a
runGraphPatterns (GraphPatterns x) g = runGraphEngine x g

instance Functor m => Functor (GraphPatterns m) where
  fmap f = GraphPatterns . ((fmap . fmap) f) . unGraphPatterns

instance Applicative m => Applicative (GraphPatterns m) where
  pure = GraphPatterns . pure . pure
  (<*>) f x = GraphPatterns $ (unGraphPatterns f) <*^*> (unGraphPatterns x)

instance Applicative m => Alternative (GraphPatterns m) where
  empty = GraphPatterns . pure $ empty
  x <|> y = GraphPatterns $ (unGraphPatterns x) <|^|> (unGraphPatterns y)

instance (Functor m, Applicative m, Monad m) => Monad (GraphPatterns m) where
  return = GraphPatterns . return . return
  x >>= k = GraphPatterns $ do
    y <- unGraphPatterns x
    -- ^ y :: GPResult a
    join <$> traverse (unGraphPatterns . k) y
    -- This monad definition is a bit dodgy as well. Must revise later.

instance (Functor m, Applicative m, Monad m) => MonadPlus (GraphPatterns m) where
  mzero = GraphPatterns . return $ mzero
  x `mplus` y = GraphPatterns $ do
    x' <- unGraphPatterns x
    y' <- unGraphPatterns y
    return $ x' `mplus` y'

-- This is not the most general type.
vertex :: (DeterminesVertex m d v) => Proxy v -> d -> GraphPatterns m v
vertex vertexProxy determiner = GraphPatterns $ do
  let vertexInfo = toEngineVertexInformation Proxy vertexProxy determiner
  engineVertex <- getVertices vertexInfo

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
  case traverse (fromEngineVertex Proxy) engineVertex of
    -- TODO make an anomaly.
    Nothing -> return $ (result . anomaly) VertexTranslationAnomaly
    Just x -> return $ (result . noAnomaly) x

edge :: (DeterminesEdge m d e) => Proxy e -> d -> GraphPatterns m e
edge edgeProxy determiner = GraphPatterns $ do
  let edgeInfo = toEngineEdgeInformation Proxy edgeProxy determiner
  engineEdge <- getEdges edgeInfo
  case traverse (fromEngineEdge Proxy) engineEdge of
    Nothing -> return $ (result . anomaly) EdgeTranslationAnomaly
    Just x -> return $ (result . noAnomaly) x

incoming
  :: forall m e v d .
     ( Edge m e
     , Vertex m v
     , DeterminesLocalEdge m v e d
     , FixDirection (EdgeDirection m v e d) In ~ In
     )
  => d
  -> v
  -> GraphPatterns m e
incoming determiner v = GraphPatterns $ do
  let edgeInfo = toEngineEdgeInformationLocal Proxy (Proxy :: Proxy v) (Proxy :: Proxy e) determiner
  engineEdge <- getEdgesIn edgeInfo (toEngineVertex Proxy v)
  case traverse (fromEngineEdge Proxy) engineEdge of
    Nothing -> return $ (result . anomaly) EdgeTranslationAnomaly
    Just x -> return $ (result . noAnomaly) x

outgoing
  :: forall m e v d .
     ( Edge m e
     , Vertex m v
     , DeterminesLocalEdge m v e d
     , FixDirection (EdgeDirection m v e d) Out ~ Out
     )
  => d
  -> v
  -> GraphPatterns m e
outgoing determiner v = GraphPatterns $ do
  let edgeInfo = toEngineEdgeInformationLocal Proxy (Proxy :: Proxy v) (Proxy :: Proxy e) determiner
  engineEdge <- getEdgesIn edgeInfo (toEngineVertex Proxy v)
  case traverse (fromEngineEdge Proxy) engineEdge of
    Nothing -> return $ (result . anomaly) EdgeTranslationAnomaly
    Just x -> return $ (result . noAnomaly) x

source
  :: forall m e .
     ( Edge m e
     )
  => e
  -> GraphPatterns m (EdgeSource m e)
source edge = GraphPatterns $ do
  let engineEdge = toEngineEdge (Proxy :: Proxy m) edge
  engineSourceVertex <- getSourceVertex engineEdge
  case fromEngineVertex (Proxy :: Proxy m) engineSourceVertex of
    Nothing -> return $ (result . anomaly) VertexTranslationAnomaly
    Just (x :: EdgeSource m e) -> return $ (result . noAnomaly) [x]

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
    Nothing -> return $ (result . anomaly) VertexTranslationAnomaly
    Just (x :: EdgeTarget m e) -> return $ (result . noAnomaly) [x]

adjacentOut
  :: forall m e d .
     ( DeterminesLocalEdge m (EdgeSource m e) e d
     , FixDirection (EdgeDirection m (EdgeSource m e) e d) Out ~ Out
     )
  => Proxy e
  -- ^ Somehow, we need the proxy to avoid ambiguity, but we never actually
  -- use the relevant value or its type... do we?
  -- Can't wrap my head around this witchcraft.
  -> d
  -> EdgeSource m e
  -> GraphPatterns m (EdgeTarget m e)
adjacentOut proxy d v = do
  outg :: e <- outgoing d v
  -- ^ This type annotation is essential; without it we get ambiguity!
  target outg

adjacentIn
  :: forall m e d .
     ( DeterminesLocalEdge m (EdgeTarget m e) e d
     , FixDirection (EdgeDirection m (EdgeTarget m e) e d) In ~ In
     )
  => Proxy e
  -> d
  -> EdgeTarget m e
  -> GraphPatterns m (EdgeSource m e)
adjacentIn proxy d v = do
  inc :: e <- incoming d v
  source inc

-- | Now we turn our attention to the GraphMutations monad for mutating a graph.
newtype GraphMutations m a = GraphMutations (GraphPatterns m a)
  deriving (Functor, Applicative, Monad)

runGraphMutations
  :: GraphEngine m
  => GraphMutations m a
  -> EngineGraph m
  -> GPResult a
runGraphMutations (GraphMutations x) = runGraphPatterns x

putVertex
  :: forall m v .
     ( Vertex m v
     )
  => v
  -> GraphMutations m v
putVertex v = GraphMutations . GraphPatterns $ do
  let engineVertex = toEngineVertex (Proxy :: Proxy m) v
  v' :: Maybe (EngineVertex m) <- insertVertex engineVertex
  case v' of
    Nothing -> return $ (result . anomaly) undefined
    Just v'' -> case fromEngineVertex (Proxy :: Proxy m) v'' of
                  Nothing -> return $ (result . anomaly) undefined 
                  Just (v''' :: v) -> return $ (result . noAnomaly) [v''']
putEdge
  :: forall m e v u .
     ( Edge m e
     , Vertex m v
     , Vertex m u
     , EdgeSource m e ~ v
     , EdgeTarget m e ~ u
     )
  => e
  -> v
  -> u
  -> GraphPatterns m e
putEdge e v u = GraphPatterns $ do
  -- TODO check edge cardinality constraints and do not insert if it's violated.
  -- That will require doing a query before a mutation.
  -- TBD Is it possible to offload this to the GraphEngine if it supports it?
  let engineEdge = toEngineEdge Proxy e
  let engineVertexV = toEngineVertex Proxy v
  let engineVertexU = toEngineVertex Proxy u
  (e' :: Maybe (EngineEdge m)) <- insertEdge engineEdge engineVertexV engineVertexU
  case e' of
    Nothing -> return $ (result . anomaly) undefined
    Just e'' -> case fromEngineEdge (Proxy :: Proxy m) e'' of
                  Nothing -> return $ (result . anomaly) undefined
                  Just (e''' :: e) -> return $ (result . noAnomaly) [e''']
