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

import Control.Applicative
import Control.Monad

import Data.Traversable (traverse, Traversable)
import Data.Foldable
import Data.Proxy

-- | Lift <*> in through another applicative.
(<*^*>) :: (Applicative m, Applicative n) => m (n (a -> b)) -> m (n a) -> m (n b)
(<*^*>) f x = (fmap (<*>) f) <*> x

-- | Lift <|> in through another alternative
(<|^|>) :: (Alternative m, Alternative n) => m (n a) -> m (n a) -> m (n a)
(<|^|>) x y = (fmap (<|>) x) <*> y

-- | The result of a query is a monad-list of values, which may be anomalies.
--   All anomalies remain in the list, but the presence of one anomaly does not
--   junk the entire query result.
newtype QueryResultT m a = QueryResultT {
    runQueryResultT :: MList m (Either Anomaly a)
  }

-- For exporting.
type QueryResult = QueryResultT
runQueryResult = runQueryResultT

instance Functor m => Functor (QueryResultT m) where
  fmap f = QueryResultT . (fmap . fmap) f . runQueryResultT

instance (Applicative m, Monad m) => Applicative (QueryResultT m) where
  pure = QueryResultT . (pure . pure)
  f <*> x = QueryResultT $ runQueryResultT f <*^*> runQueryResultT x

instance (Applicative m, Monad m) => Monad (QueryResultT m) where
  return = QueryResultT . (return . return)
  x >>= k = QueryResultT $ do
    x' <- runQueryResultT x
    -- x' :: Either Anomaly a
    mbind $ (fmap (runQueryResultT . k)) x'

mbind :: (Applicative m, Monad m) => Either Anomaly (MList m (Either Anomaly a)) -> MList m (Either Anomaly a)
mbind (Left l) = return $ Left l
mbind (Right list) = list

resultOk :: (Applicative m, Monad m) => [a] -> QueryResultT m a
resultOk = QueryResultT . toMList . fmap Right

resultNotOk :: (Applicative m, Monad m) => Anomaly -> QueryResultT m a
resultNotOk = QueryResultT . return . Left

-- Lift some monadic value into QueryResult.
liftQ :: (Functor m, Monad m) => m a -> QueryResultT m a
liftQ x = QueryResultT $ (fmap Right x) `mlistCons` mlistEmpty

--liftListQ :: (Functor m, Monad m) => m [a] -> QueryResultT m a
liftListQ val = QueryResultT $ do
  x <- convertToMList val
  return $ Right x

-- | Our query EDSL is just a new name for the QueryResult monad, over some
--   other monad, which in practice shall be a GraphEngine.
newtype GraphQueries m a = GraphQueries {
    runGraphQueries :: QueryResultT m a
  }

instance Functor m => Functor (GraphQueries m) where
  fmap f = GraphQueries . fmap f . runGraphQueries

instance (Functor m, Applicative m, Monad m) => Applicative (GraphQueries m) where
  pure = GraphQueries . pure
  (<*>) f x = GraphQueries $ (runGraphQueries f) <*> (runGraphQueries x)

-- How oh how do we define this monad?
instance (Functor m, Applicative m, Monad m) => Monad (GraphQueries m) where
  return = GraphQueries . return
  x >>= k = GraphQueries $ do
    y <- runGraphQueries x
    -- ^ y :: QueryResult a
    (runGraphQueries . k) y

instance (Functor m, Applicative m, Monad m) => Alternative (GraphQueries m) where
  empty = GraphQueries $ QueryResultT mlistEmpty
  x <|> y = GraphQueries $ QueryResultT (x' `mlistAppend` y')
    where x' = runQueryResultT . runGraphQueries $ x
          y' = runQueryResultT . runGraphQueries $ y

instance (Functor m, Applicative m, Monad m) => MonadPlus (GraphQueries m) where
  mzero = GraphQueries $ QueryResultT mlistEmpty
  mplus = (<|>)

-- | This type carries an EngineVertex and a Vertex. It shall be produced
--   by our DSL primitives. It is here so that we don't have to continually
--   convert to and from EngineVertex.
data V m v = forall ev . (ev ~ EngineVertex m v) => V ev v

-- | Like V, this type is here to prevent superfluous conversions from
--   engine types.
data E m e = forall ee . (ee ~ EngineEdge m e) => E ee e

-- Projection from V onto its EngineVertex.
engineV
  :: forall m v ev .
     ( ev ~ EngineVertex m v
     )
  => V m v
  -> ev
engineV (V ev _) = ev

-- Projection from E onto its EngineEdge.
engineE
  :: forall m e ee .
     ( ee ~ EngineEdge m e
     )
  => E m e
  -> ee
engineE (E ee _) = ee

-- | Projection from V onto its Vertex.
v
  :: forall m v .
     ()
  => V m v
  -> v
v (V _ x) = x

-- | Projection from E onto its Edge.
e
  :: forall m e .
     ()
  => E m e
  -> e
e (E _ x) = x

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
     )
  => ev
  -> GraphQueries m (V m v)
v' engineVertex = GraphQueries $ do
  let maybeVertex :: Maybe v = fromEngineVertex engineVertex
  case maybeVertex of
    Nothing -> resultNotOk VertexTranslationAnomaly
    Just vertex -> resultOk [V engineVertex vertex]

-- Injection into E; this can only be done within the GraphQueries
-- monad, because an EngineEdge must also be defined!
e'
  :: forall m e ee .
     ( EngineEdge m e ~ ee
     , Edge m e
     )
  => ee
  -> GraphQueries m (E m e)
e' engineEdge = GraphQueries $ do
  let maybeEdge :: Maybe e = fromEngineEdge engineEdge
  case maybeEdge of
    Nothing -> resultNotOk EdgeTranslationAnomaly
    Just edge -> resultOk [E engineEdge edge]

-- This is not the most general type.
vertex
  :: forall m d v .
     ( DeterminesVertex m d v
     )
  => Proxy v
  -> d
  -> GraphQueries m (V m v)
vertex vertexProxy determiner = GraphQueries $ do
  let vertexInfo = toEngineVertexInformation (Proxy :: Proxy m) vertexProxy determiner
  engineVertex <- liftListQ $ getVertices vertexInfo
  runGraphQueries $ v' engineVertex
  -- Must check for a uniqueness anomaly here!!!
  -- TODO FIXME
  -- You can probably accomplish this by doing
  --   engineVertices <- liftQ $ getVertices vertexInfo
  --   makeChecks engineVertices
  --   engineVertex <- liftListQ $ return engineVertices
  --   runGraphQueries $ v engineVertex

edge
  :: forall m d e .
     ( DeterminesEdge m d e
     )
  => Proxy e
  -> d
  -> GraphQueries m (E m e)
edge edgeProxy determiner = GraphQueries $ do
  let edgeInfo = toEngineEdgeInformation (Proxy :: Proxy m) edgeProxy determiner
  engineEdge <- liftListQ $ getEdges edgeInfo
  runGraphQueries $ e' engineEdge

incoming
  :: forall m e v d .
     ( Edge m e
     , Vertex m v
     , DeterminesLocalEdge m v e d
     , FixDirection (EdgeDirection m v e d) In ~ In
     )
  => d
  -> V m v
  -> GraphQueries m (E m e)
incoming determiner vertex = GraphQueries $ do
  let edgeInfo = toEngineEdgeInformationLocal Proxy (Proxy :: Proxy v) (Proxy :: Proxy e) determiner
  engineEdges <- liftQ $ getEdgesIn edgeInfo (engineV vertex)
  -- TODO FIXME check for edge cardinality anomaly
  engineEdge <- liftListQ $ return engineEdges
  runGraphQueries $ e' engineEdge

outgoing
  :: forall m e v d .
     ( Edge m e
     , Vertex m v
     , DeterminesLocalEdge m v e d
     , FixDirection (EdgeDirection m v e d) Out ~ Out
     )
  => d
  -> V m v
  -> GraphQueries m (E m e)
outgoing determiner vertex = GraphQueries $ do
  let edgeInfo = toEngineEdgeInformationLocal Proxy (Proxy :: Proxy v) (Proxy :: Proxy e) determiner
  engineEdges <- liftQ $ getEdgesIn edgeInfo (engineV vertex)
  -- TODO FIXME check for edge cardinaltiy anomaly
  engineEdge <- liftListQ $ return engineEdges
  runGraphQueries $ e' engineEdge


source
  :: forall m e .
     ( Edge m e
     )
  => E m e
  -> GraphQueries m (V m (EdgeSource m e))
source edge = GraphQueries $ do
  sourceVertex <- liftQ $ getSourceVertex (engineE edge)
  case sourceVertex of
    Nothing -> resultNotOk undefined -- TODO proper anomaly.
    Just x -> runGraphQueries $ v' x

target
  :: forall m e .
     ( Edge m e
     )
  => E m e
  -> GraphQueries m (V m (EdgeTarget m e))
target edge = GraphQueries $ do
  targetVertex <- liftQ $ getTargetVertex (engineE edge)
  case targetVertex of
    Nothing -> resultNotOk undefined -- TODO proper anomaly
    Just x -> runGraphQueries $ v' x

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
  -> V m (EdgeSource m e)
  -> GraphQueries m (V m (EdgeTarget m e))
adjacentOut proxy d vertex = do
  outg :: E m e <- outgoing d vertex
  -- ^ This type annotation is essential; without it we get ambiguity!
  --target $ E (Left outg)
  target outg

adjacentIn
  :: forall m e d .
     ( DeterminesLocalEdge m (EdgeTarget m e) e d
     , FixDirection (EdgeDirection m (EdgeTarget m e) e d) In ~ In
     )
  => Proxy e
  -> d
  -> V m (EdgeTarget m e)
  -> GraphQueries m (V m (EdgeSource m e))
adjacentIn proxy d vertex = do
  inc :: E m e <- incoming d vertex
  source inc

-- | Now we turn our attention to the GraphMutations monad for mutating a graph.
--   This monad is also a GraphQueries monad, but with an added bonus: when
--   you run it, you get an EngineGraph as well. This is useful for pure
--   GraphEngines; without it, the mutations could never be observed outside
--   of the GraphMutations monad.
--
--   One option: define a new Monad, and then injections which allow us to
--   update the graph uniformly for all branches of the GPResult's underlying
--   list.
--   Another option (maybe?): define the mutations monad separately, without
--   querying capabilities, and then take their product?
--
--   Hm, why not just StateT over GraphQueries!?!?!?!?!?!
--   Nope, that's not what we want... well, maybe it is? The issue is that
--   the stateful part cannot be used by the GraphQueries reader monad. That
--   means that the reads cannot use writes which have been expressed in the
--   same monadic value. Is this what we want?

-- Ok new plan. Start with the monad which can do only mutations.
-- And suppose we have this. Then what? It's not so obvious how to make a monad
-- out of the two, which can do updates AND queries. Isn't there such a thing
-- as a product monad? Yes, of course, so we can just take
--   
--   newtype GraphPatterns m a = GraphPatterns {
--       _unGraphPatterns :: Product (GraphQueries m) (GraphMutations m) a
--     } deriving (Functor, Applicative, Monad)
--
--newtype GraphMutations s m a = GraphMutations {
--    _unGraphMutations :: StateT s (EitherT m) a
--  } deriving (Functor, Applicative, Monad)

newtype GraphMutations m a = GraphMutations {
    runGraphMutations :: m (Either Anomaly a)
  }

instance Functor m => Functor (GraphMutations m) where
  fmap f = GraphMutations . (fmap . fmap) f . runGraphMutations

instance (Applicative m, Monad m) => Applicative (GraphMutations m) where
  pure = GraphMutations . pure . pure
  f <*> x = GraphMutations $ runGraphMutations f <*^*> runGraphMutations x

instance (Applicative m, Monad m) => Monad (GraphMutations m) where
  return = GraphMutations . return . return
  x >>= k = GraphMutations $ do
    x' <- runGraphMutations x
    case x' of
      Left l -> return $ Left l
      Right x'' -> runGraphMutations $ k x''

putVertex
  :: forall m v .
     ( Vertex m v
     )
  => v
  -> GraphMutations m (EngineVertex m v)
putVertex v = GraphMutations $ do
  let engineVertex :: EngineVertexInsertion m v = toEngineVertexInsertion v
  b <- insertVertex engineVertex
  case b of
    Nothing -> return $ Left VertexInsertionAnomaly
    Just x -> return $ Right x

putEdge
  :: forall m e v u v' u' .
     ( Edge m e
     , Vertex m u
     , Vertex m v
     , SubVertex m u u'
     , SubVertex m v v'
     , EdgeSource m e ~ u'
     , EdgeTarget m e ~ v'
     )
  => e
  -> EngineVertex m u
  -> EngineVertex m v
  -> GraphMutations m (EngineEdge m e)
putEdge e u v = GraphMutations $ do
  -- TODO check edge cardinality constraints and do not insert if it's violated.
  -- That will require doing a query before a mutation.
  -- TBD Is it possible to offload this to the GraphEngine if it supports it?
  let edgeInsertion :: EngineEdgeInsertion m e = toEngineEdgeInsertion e
  success <- insertEdge edgeInsertion u v
  case success of
    Nothing -> return $ Left EdgeInsertionAnomaly
    Just x -> return $ Right x

-- | The all-powerful GraphPatterns monad is just GraphQueries with injectors
--   for GraphQueries and GraphMutations.
--   NB your GraphMutations happen in each branch of the query!
newtype GraphPatterns m a = GraphPatterns {
    unGraphPatterns :: GraphQueries m a
  } deriving (Functor, Applicative, Monad, Alternative, MonadPlus)

type GraphPatternsResult = QueryResultT

query :: GraphQueries m a -> GraphPatterns m a
query = GraphPatterns

mutation :: Monad m => GraphMutations m a -> GraphPatterns m a
mutation m = GraphPatterns . GraphQueries . QueryResultT $ x
  where x = mlistCons (runGraphMutations m) mlistEmpty

runGraphPatterns :: GraphPatterns m a -> GraphPatternsResult m a
runGraphPatterns = runGraphQueries . unGraphPatterns

runGraphPatterns' :: (Applicative m, Monad m) => GraphPatterns m a -> m [Either Anomaly a]
runGraphPatterns' = fromMList . runQueryResult . runGraphPatterns
