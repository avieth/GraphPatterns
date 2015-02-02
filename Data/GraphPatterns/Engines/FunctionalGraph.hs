{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}

module Data.GraphPatterns.Engines.FunctionalGraph (
    FunctionalGraph
  , runFunctionalGraph
  , FGraph
  , VertexLabel(..)
  , EdgeLabel(..)
  , EngineVertex(..)
  , EngineEdge(..)
  , EngineVertexInsertion(..)
  , EngineEdgeInsertion(..)
  , EngineVertexInformation(..)
  , EngineEdgeInformation(..)
  , MapKey
  , MapValue
  ) where

import qualified Data.Map as M
import Control.Applicative
import Control.Monad.Trans.State
import Control.Monad.Identity

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree

import Data.GraphPatterns.GraphEngine

-- | A functional graph which carries our labels.
type FGraph = Gr VertexLabel EdgeLabel

-- | Each vertex is labelled with an arbitrary map of properties.
newtype VertexLabel = VertexLabel {
    vertexProperties :: M.Map MapKey MapValue
  } deriving (Show)

-- | Each edge is labelled with an arbitrary map of properties
newtype EdgeLabel = EdgeLabel {
    edgeProperties :: M.Map MapKey MapValue
  } deriving (Show)

-- | For serious use, we would probably want to use ByteString or maybe Text
--   for these. But then, if speed is our concern, we'll probably choose an
--   impure graph implementation like Neo4j or Titan.
type MapKey = String
type MapValue = String

-- | A "less than" predicate on properties. a <= b if and only if every key
--   in a is present in b and maps to the same value; it's the subset
--   predicate if we treat a map as a set of {(key, value)}
--   Note that this is not just the Ord instance of M.Map; that one gives
--   a total order but this one is partial.
--   TODO (low priority) bring in Data.Poset and describe the ordering
--   rather than just identifying less thans through a Bool.
matchProperties :: M.Map MapKey MapValue -> M.Map MapKey MapValue -> Bool
matchProperties k0 k1 = M.foldrWithKey combine True k0
  where combine k v b = b && maybe False ((==) v) (M.lookup k k1)

-- | The functional graph monad keeps a pure FGraph in state, so that we can
--   query and update.
newtype FunctionalGraph a = FG {
    runFG :: StateT FGraph Identity a
  } deriving (Functor, Applicative, Monad)

runFunctionalGraph :: FGraph -> FunctionalGraph a -> (a, FGraph)
runFunctionalGraph graph = runIdentity . (flip runStateT) graph . runFG

instance GraphEngine FunctionalGraph where

  type EngineGraph FunctionalGraph = FGraph

  data EngineVertex FunctionalGraph v = FGVertex (LNode VertexLabel)

  data EngineEdge FunctionalGraph e = FGEdge (LEdge EdgeLabel)

  data EngineVertexInsertion FunctionalGraph v = FGVertexInsertion VertexLabel

  data EngineEdgeInsertion FunctionalGraph e = FGEdgeInsertion EdgeLabel

  data EngineVertexInformation FunctionalGraph v = FGVertexInfo (M.Map MapKey MapValue)

  data EngineEdgeInformation FunctionalGraph e = FGEdgeInfo (M.Map MapKey MapValue)

  getTargetVertex (FGEdge (_, node, _)) = FG $ do
      graph <- get
      return $ fmap (\label -> FGVertex (node, label)) (lab graph node)

  getSourceVertex (FGEdge (node, _, _)) = FG $ do
      graph <- get
      return $ fmap (\label -> FGVertex (node, label)) (lab graph node)

  getVertices (FGVertexInfo properties) = FG $ do
      graph <- get
      return $ ufold combine [] graph
        where combine (_, n, label, _) xs =
                if matchProperties properties (vertexProperties label)
                then FGVertex (n, label) : xs
                else xs

  -- I don't know if there's a more efficient way to do this. We just grab
  -- all labelled edges and check each one.
  getEdges (FGEdgeInfo properties) = FG $ do
      graph <- get
      let edges = labEdges graph
      return $ foldr combine [] edges
        where combine e@(_, _, label) xs =
                if matchProperties properties (edgeProperties label)
                then FGEdge e : xs
                else xs

  getEdgesOut (FGEdgeInfo properties) (FGVertex (n, _)) = FG $ do
      graph <- get
      -- Yes, this may error; we'll fix FGL later.
      let edges = out graph . node' . context graph $ n
      return $ map FGEdge $ filter (\(_,_,l) -> matchProperties properties (edgeProperties l)) edges

  getEdgesIn (FGEdgeInfo properties) (FGVertex (n, _)) = FG $ do
      graph <- get
      let edges = inn graph . node' . context graph $ n
      return $ map FGEdge $ filter (\(_,_,l) -> matchProperties properties (edgeProperties l)) edges

  insertVertex (FGVertexInsertion label) = FG $ do
      graph <- get
      let nodes = newNodes 1 graph
      case nodes of
        [] -> return Nothing
        node : _ -> do
            put $ insNode (node, label) graph
            return $ Just (FGVertex (node, label))

  insertEdge (FGEdgeInsertion label) (FGVertex (u, _)) (FGVertex (v, _)) = FG $ do
      -- If we've done anything right, the typechecker should have guaranteed
      -- that the two vertices are of the right type.
      graph <- get
      put $ insEdge (u, v, label) graph
      return $ Just (FGEdge (u, v, label))
