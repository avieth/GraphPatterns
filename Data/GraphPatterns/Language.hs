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
  --, hopOutgoing
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
  :: (GraphEngine m, EdgeLabel l)
  => l
  -> Vertex m
  -> m (EdgeTraversalResult Incoming (EdgeCardinality l) (Edge m))
incoming = getEdgesIn

outgoing
  :: (GraphEngine m, EdgeLabel l)
  => l
  -> Vertex m
  -> m (EdgeTraversalResult Outgoing (EdgeCardinality l) (Edge m))
outgoing = getEdgesOut

source :: GraphEngine m => Edge m -> m (Vertex m)
source = getSourceVertex

target :: GraphEngine m => Edge m -> m (Vertex m)
target = getTargetVertex

-- WART: I want this type signature...
--adjacentOut
--  :: (GraphEngine m, EdgeLabel l)
--  => l
--  -> Vertex m
--  -> m (EdgeTraversalResult Outgoing (EdgeCardinality l) (Vertex m))
-- ... but I have to give
adjacentOut
  :: (Traversable t, GraphEngine m, EdgeLabel l, EdgeTraversalResult Outgoing (EdgeCardinality l) (Edge m) ~ t (Edge m))
  => l
  -> Vertex m
  -> m (t (Vertex m))
-- because I haven't told GHC that every image of EdgeTraversalResult is a
-- Traversable. Is that possible?
adjacentOut el v = do
  es <- getEdgesOut el v
  -- Shit, we don't know what the type of es is!
  -- We've got to dispatch this at the type level, as part of a class perhaps?
  -- Maybe we could have some function
  --
  -- class Magic c where
  --   magic :: ([a] -> b) -> EdgeTraversalResult _ c _ -> b
  -- 
  -- instance Magic Incoming ManyToMany where
  --   magic f x = mapM
  --
  -- We know that es is a monad (either Identity or []) so we can map into
  -- it
  traverse getTargetVertex es

adjacentIn
  :: (Traversable t, GraphEngine m, EdgeLabel l, EdgeTraversalResult Incoming (EdgeCardinality l) (Edge m) ~ t (Edge m))
  => l
  -> Vertex m
  -> m (t (Vertex m))
adjacentIn el v = getEdgesIn el v >>= traverse getSourceVertex

adjacent
  :: (Traversable t, GraphEngine m, EdgeLabel l, EdgeTraversalResult Both (EdgeCardinality l) (Edge m) ~ t (Edge m))
  => l
  -> Vertex m
  -> m (t (Vertex m))
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

-- | A traversal plus a join on the inner monad (the traversable is also a
--   monad).
flatTraverse
  :: (Traversable t, Monad t, Applicative f)
  => (a -> f (t b))
  -> t a
  -> f (t b)
flatTraverse = (doublefmap join) . traverse
  where doublefmap = fmap . fmap

-- Hop n times on incoming edges.
hopIncoming
  :: ( Monad t, Traversable t, GraphEngine m, EdgeLabel l
     , EdgeTraversalResult Incoming (EdgeCardinality l) (Edge m) ~ t (Edge m))
  => Int
  -> l
  -> Vertex m
  -> m (t (Vertex m))
hopIncoming n l v
  | n > 0 = adjacentIn l v >>= flatTraverse (hopIncoming (n-1) l)
  -- We return v even if n < 0. Better than error? Not sure.
  | otherwise = return . return $ v

-- Hop n times on outgoing edges.
hopOutgoing
  :: ( Monad t, Traversable t, GraphEngine m, EdgeLabel l
     , EdgeTraversalResult Outgoing (EdgeCardinality l) (Edge m) ~ t (Edge m))
  => Int
  -> l
  -> Vertex m
  -> m (t (Vertex m))
hopOutgoing n l v
  | n > 0 = adjacentOut l v >>= flatTraverse (hopOutgoing (n-1) l)
  -- We return v even if n < 0. Better than error? Not sure.
  | otherwise = return . return $ v
