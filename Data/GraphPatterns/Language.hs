{-# LANGUAGE TypeFamilies #-}

module Data.GraphPatterns.Language (
    vertex
  , edge
  , incoming
  , outgoing
  , source
  , target
  , adjacentOut
  , adjacentIn
  , adjacent
  , hopIncoming
  , hopOutgoing
  ) where

import Data.GraphPatterns.GraphEngine

import Control.Applicative ((<$>), (<*>), Applicative)
import Control.Monad (join)
import Data.Traversable (traverse, Traversable)

-- Some declarative sugar for GraphEngine methods (no verbs).

vertex :: GraphEngine m => VertexId m -> m (Maybe (Vertex m))
vertex = getVertexById

edge :: GraphEngine m => EdgeId m -> m (Maybe (Edge m))
edge = getEdgeById

incoming
  :: ( GraphEngine m
     , HandlesAnomaly a
     , EdgeTraversalResult Incoming (EdgeCardinality l) (Edge m) ~ a (Edge m))
  => l
  -> Vertex m
  -> m (Either Anomaly (a (Edge m)))
incoming = getEdgesIn

outgoing
  :: ( GraphEngine m
     , HandlesAnomaly a
     , EdgeTraversalResult Outgoing (EdgeCardinality l) (Edge m) ~ a (Edge m))
  => l
  -> Vertex m
  -> m (Either Anomaly (a (Edge m)))
outgoing = getEdgesOut

source :: GraphEngine m => Edge m -> m (Vertex m)
source = getSourceVertex

target :: GraphEngine m => Edge m -> m (Vertex m)
target = getTargetVertex

adjacentOut el v = getEdgesOut el v >>= traverse (fmap getTargetVertex)

adjacentIn el v = getEdgesIn el v >>= traverse (fmap getSourceVertex)

adjacent el v = do
  esIn <- getEdgesIn el v
  esOut <- getEdgesOut el v
  -- Oh no, what are the types!?!?!
  --(++) <$> mapM getSourceVertex esIn <*> mapM getTargetVertex esOut
  -- Perhaps we need a class
  --
  --   class Magic a b c where
  --     combine a b :: a -> b -> c
  --
  --   instance Magic (One a) (Many a) (Many a) where
  --     combine (One x) (Many ys) = Many $ (runIdentity x) : ys
  --
  --   instance Magic (Many a) (One a) (Many a) where
  --     combine (Many xs) (One y) = Many $ xs ++ [runIdentity y]
  --
  -- ...
  return undefined

-- Hop n times on incoming edges.
hopIncoming n l v
  | n > 0 = adjacentIn l v >>= \r -> case r of
      Left y -> return $ Left y
      -- The type in Right s, s :: t a
      -- and we want to produce something of type
      --
      --   m (Either Anomaly (t a))
      -- 
      -- Ok, so we traverse the t, doing hopIncoming (n-1) l, which
      -- gives us something of type
      --
      --   traverse traverser s :: m (t (Either Anomaly (t a)))
      --
      -- and then we propagateAnomaly to get an
      --
      --   m (Either Anomaly (t (t a)))
      --
      -- followed by a join in the innermost monad to get what we need
      --
      --   m (Either Anomaly (t a))
      --
      -- and it's still all good.
      Right s -> (fmap join) . propagateAnomaly <$> (traverse traverser s)
        where traverser x = hopIncoming (n-1) l x

  -- We return v even if n < 0. Better than error? Not sure.
  -- Note the 3 returns. Once for the GraphEngine monad, once for
  -- Either Anomaly, and once for the EdgeTraversalResult image.
  | otherwise = return . return . return $ v

-- Hop n times on outgoing edges.
hopOutgoing n l v
  | n > 0 = adjacentOut l v >>= \r -> case r of
      Left y -> return $ Left y
      Right s -> (fmap join) . propagateAnomaly <$> (traverse traverser s)
        where traverser x = hopOutgoing (n-1) l x

-- | Propagate an Anomaly out through a traversable.
propagateAnomaly :: Traversable t => t (Either Anomaly a) -> Either Anomaly (t a)
propagateAnomaly = traverse id
