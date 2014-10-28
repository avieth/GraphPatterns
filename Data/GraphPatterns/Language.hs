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

  -- TODO
  , GraphPatterns
  , runGraphPatterns
  , query 
  -- ^ injects a GraphQueries into GraphPatterns
  , mutation
  -- ^ injects a GraphMutations into GraphPatterns

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

-- | We begin with a special monad-list type, as seen in the ListT done right
--   alternative http://www.haskell.org/haskellwiki/ListT_done_right_alternative
newtype MList m a = MList {
    runMList :: m (Maybe (a, MList m a))
  }

instance Functor m => Functor (MList m) where
  fmap f = MList . (fmap . fmap) f' . runMList
    where f' (x, next) = (f x, fmap f next)

instance (Applicative m, Monad m) => Applicative (MList m) where
  pure x = MList $ pure (Just (x, MList $ pure Nothing))
  f <*> x = ap f x

instance (Applicative m, Monad m) => Monad (MList m) where
  return = pure
  x >>= k = MList $ do
    x' <- runMList x
    case x' of
      Nothing -> return Nothing
      Just (x'', next) -> runMList $ (k x'') `mlistAppend` (next >>= k)

mlistAppend :: Monad m => MList m a -> MList m a -> MList m a
mlistAppend first second = MList $ do
  head <- runMList first
  case head of
    Nothing -> runMList second
    Just (h, rest) -> return $ Just (h, mlistAppend rest second)

mlistEmpty :: Monad m => MList m a
mlistEmpty = MList $ return Nothing

mlistCons :: Monad m => m a -> MList m a -> MList m a
mlistCons x rest = MList $ do
  x' <- x
  return $ Just (x', rest)

toMList :: Monad m => [a] -> MList m a
--toMList [] = MList $ return Nothing
--toMList (x:xs) = MList $ return (Just (x, toMList xs))
toMList [] = mlistEmpty
toMList (x:xs) = mlistCons (return x) (toMList xs)

fromMList :: (Applicative m, Monad m) => MList m a -> m [a]
fromMList x = do
  head <- runMList x
  case head of
    Nothing -> return []
    Just (x', rest) -> (:) <$> return x' <*> fromMList rest

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

mjoin :: Monad m
      => MList m (Either Anomaly (MList m (Either Anomaly a)))
      -> MList m (Either Anomaly a)
mjoin x = MList $ do
  x' <- runMList x
  case x' of
    Nothing -> return Nothing
    Just (y, rest) -> case y of
      -- ^ y :: Either Anomaly (MList m (Either Anomaly a))
      --   rest :: MList m (Either Anomaly (MList m (Either Anomaly a)))
      Left l -> runMList $ (return (Left l)) `mlistCons` (mjoin rest)
      Right list -> runMList $ list `mlistAppend` (mjoin rest)


resultOk :: (Applicative m, Monad m) => [a] -> QueryResultT m a
resultOk = QueryResultT . toMList . fmap Right

resultNotOk :: (Applicative m, Monad m) => Anomaly -> QueryResultT m a
resultNotOk = QueryResultT . return . Left

liftQ x = QueryResultT $ (fmap Right x) `mlistCons` mlistEmpty

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

-- This is not the most general type.
vertex
  :: forall m d v .
     ( DeterminesVertex m d v
     )
  => Proxy v
  -> d
  -> GraphQueries m v
vertex vertexProxy determiner = GraphQueries $ do
  let vertexInfo = toEngineVertexInformation (Proxy :: Proxy m) vertexProxy determiner
  engineVertices <- liftQ $ getVertices vertexInfo
  -- Must check for a uniqueness anomaly here!!!
  -- TODO FIXME
  --
  -- Here we replace all Nothings that come out of fromEngineVertex with
  -- a VertexTranslationAnomaly.
  QueryResultT (toMList $ fmap mapper engineVertices)
    where mapper = maybe (Left VertexTranslationAnomaly) Right . fromEngineVertex (Proxy :: Proxy m)

edge
  :: forall m d e .
     ( DeterminesEdge m d e
     )
  => Proxy e
  -> d
  -> GraphQueries m e
edge edgeProxy determiner = GraphQueries $ do
  let edgeInfo = toEngineEdgeInformation (Proxy :: Proxy m) edgeProxy determiner
  engineEdges <- liftQ $ getEdges edgeInfo
  QueryResultT (toMList $ fmap mapper engineEdges)
    where mapper = maybe (Left EdgeTranslationAnomaly) Right . fromEngineEdge (Proxy :: Proxy m)

incoming
  :: forall m e v d .
     ( Edge m e
     , Vertex m v
     , DeterminesLocalEdge m v e d
     , FixDirection (EdgeDirection m v e d) In ~ In
     )
  => d
  -> v
  -> GraphQueries m e
incoming determiner v = GraphQueries $ do
  let edgeInfo = toEngineEdgeInformationLocal Proxy (Proxy :: Proxy v) (Proxy :: Proxy e) determiner
  engineEdges <- liftQ $ getEdgesIn edgeInfo (toEngineVertex Proxy v)
  -- TODO FIXME check for edge cardinality anomaly
  QueryResultT (toMList $ fmap mapper engineEdges)
    where mapper = maybe (Left EdgeTranslationAnomaly) Right . fromEngineEdge (Proxy :: Proxy m)

outgoing
  :: forall m e v d .
     ( Edge m e
     , Vertex m v
     , DeterminesLocalEdge m v e d
     , FixDirection (EdgeDirection m v e d) Out ~ Out
     )
  => d
  -> v
  -> GraphQueries m e
outgoing determiner v = GraphQueries $ do
  let edgeInfo = toEngineEdgeInformationLocal Proxy (Proxy :: Proxy v) (Proxy :: Proxy e) determiner
  engineEdges <- liftQ $ getEdgesIn edgeInfo (toEngineVertex Proxy v)
  -- TODO FIXME check for edge cardinaltiy anomaly
  QueryResultT (toMList $ fmap mapper engineEdges)
    where mapper = maybe (Left EdgeTranslationAnomaly) Right . fromEngineEdge (Proxy :: Proxy m)

source
  :: forall m e .
     ( Edge m e
     )
  => e
  -> GraphQueries m (EdgeSource m e)
source edge = GraphQueries $ do
  let engineEdge = toEngineEdge (Proxy :: Proxy m) edge
  engineSourceVertex <- liftQ $ getSourceVertex engineEdge
  case fromEngineVertex (Proxy :: Proxy m) engineSourceVertex of
    Nothing -> resultNotOk VertexTranslationAnomaly
    Just (x :: EdgeSource m e) -> resultOk [x]

target
  :: forall m e .
     ( Edge m e
     )
  => e
  -> GraphQueries m (EdgeTarget m e)
target edge = GraphQueries $ do
  let engineEdge = toEngineEdge Proxy edge
  engineTargetVertex <- liftQ $ getTargetVertex engineEdge
  case fromEngineVertex (Proxy :: Proxy m) engineTargetVertex of
    Nothing -> resultNotOk VertexTranslationAnomaly
    Just (x :: EdgeTarget m e) -> resultOk [x]

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
  -> GraphQueries m (EdgeTarget m e)
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
  -> GraphQueries m (EdgeSource m e)
adjacentIn proxy d v = do
  inc :: e <- incoming d v
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
  -> GraphMutations m ()
putVertex v = GraphMutations $ do
  let engineVertex = toEngineVertex (Proxy :: Proxy m) v
  b <- insertVertex engineVertex
  case b of
    False -> return $ Left VertexInsertionAnomaly
    True -> return $ Right ()

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
  -> GraphMutations m ()
putEdge e v u = GraphMutations $ do
  -- TODO check edge cardinality constraints and do not insert if it's violated.
  -- That will require doing a query before a mutation.
  -- TBD Is it possible to offload this to the GraphEngine if it supports it?
  let engineEdge = toEngineEdge Proxy e
  let engineVertexV = toEngineVertex Proxy v
  let engineVertexU = toEngineVertex Proxy u
  success <- insertEdge engineEdge engineVertexV engineVertexU
  case success of
    False -> return $ Left EdgeInsertionAnomaly
    True -> return $ Right ()

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
