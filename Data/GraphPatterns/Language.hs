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

import Control.Applicative ((<$>), (<*>))

-- Some declarative sugar for GraphEngine methods (no verbs).

vertex :: GraphEngine m => VertexId m -> m (Maybe (Vertex m))
vertex = getVertexById

edge :: GraphEngine m => EdgeId m -> m (Maybe (Edge m))
edge = getEdgeById

incoming :: GraphEngine m => EdgeLabel m -> Vertex m -> m [Edge m]
incoming = getEdgesIn

outgoing :: GraphEngine m => EdgeLabel m -> Vertex m -> m [Edge m]
outgoing = getEdgesOut

source :: GraphEngine m => Edge m -> m (Vertex m)
source = getSourceVertex

target :: GraphEngine m => Edge m -> m (Vertex m)
target = getTargetVertex

adjacentOut :: GraphEngine m => EdgeLabel m -> Vertex m -> m [Vertex m]
adjacentOut el v = do
  es <- getEdgesOut el v
  mapM getTargetVertex es

adjacentIn :: GraphEngine m => EdgeLabel m -> Vertex m -> m [Vertex m]
adjacentIn el v = do
  es <- getEdgesIn el v
  mapM getSourceVertex es

adjacent :: GraphEngine m => EdgeLabel m -> Vertex m -> m [Vertex m]
adjacent el v = do
  esIn <- getEdgesIn el v
  esOut <- getEdgesOut el v
  (++) <$> mapM getSourceVertex esIn <*> mapM getTargetVertex esOut

-- Hop n times on incoming edges.
hopIncoming :: GraphEngine m => Int -> EdgeLabel m -> Vertex m -> m ([Vertex m])
hopIncoming n l v
  | n > 0 = do
    ws <- adjacentIn l v
    concat <$> mapM (hopIncoming (n-1) l) ws
  -- We return v even if n < 0. Better than error? Not sure.
  | otherwise = return [v]

-- Hop n times on outgoing edges.
hopOutgoing :: GraphEngine m => Int -> EdgeLabel m -> Vertex m -> m ([Vertex m])
hopOutgoing n l v
  | n > 0 = do
    ws <- adjacentOut l v
    concat <$> mapM (hopIncoming (n-1) l) ws
  -- We return v even if n < 0. Better than error? Not sure.
  | otherwise = return [v]
