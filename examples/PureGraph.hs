{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module PureGraph (
    PureGraph
  ) where

import Data.Graph as G
import Data.Void (Void)
import Control.Applicative (Applicative)

import Data.GraphPatterns.GraphEngine

newtype PureGraph a = PureGraph (G.Graph -> a)
  deriving (Functor, Applicative, Monad)

-- | A description of how the containers Data.Graph type can be used as a
--   GraphEngine.
instance GraphEngine PureGraph where
  type Graph PureGraph = G.Graph
  data Vertex PureGraph = PureGraphVertex G.Vertex
  data Edge PureGraph = PureGraphEdge G.Edge
  type EdgeId PureGraph = Void
  type VertexId PureGraph = G.Vertex
  data EngineEdgeLabel PureGraph = PureGraphEdgeLabel ()
  runGraphEngine (PureGraph f) g = f g
  getVertexById vid = PureGraph $ \g -> case (elem vid) $ vertices g of
    True -> Just $ PureGraphVertex vid
    False -> Nothing
  -- | This one can never be called, because EdgeId PureGraph = Void!
  getEdgeById = undefined
  getEdgesOut l v = PureGraph $ \g ->
      handleAnomaly Outgoing l v (map PureGraphEdge (filter predicate $ edges g))
    where predicate (x, _) = x ^== v
  getEdgesIn l v = PureGraph $ \g ->
      handleAnomaly Incoming l v (map PureGraphEdge (filter predicate $ edges g))
    where predicate (_, x) = x ^== v
  getTargetVertex (PureGraphEdge (_, i)) = return $ PureGraphVertex i
  getSourceVertex (PureGraphEdge (i, _)) = return $ PureGraphVertex i

(^==) x (PureGraphVertex i) = x == i
