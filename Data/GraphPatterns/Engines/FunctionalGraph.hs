{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE KindSignatures #-}

module Data.GraphPatterns.Engines.FunctionalGraph (

    FunctionalGraph(..)
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
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.GraphPatterns.MList
import Data.Functor.Identity

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
--   TODO (low priority) bring in Algebra.PartialOrd from the lattices package
--   and describe the ordering rather than just identifying less thans through
--   a Bool.
matchProperties :: M.Map MapKey MapValue -> M.Map MapKey MapValue -> Bool
matchProperties k0 k1 = M.foldrWithKey combine True k0
  where combine k v b = b && maybe False ((==) v) (M.lookup k k1)

data FunctionalGraph (m :: * -> *) = FG

runFunctionalGraph
  :: ( Functor m
     , Monad m
     )
  => FGraph
  -> GraphEngineMonad (FunctionalGraph m) t
  -> m ([t], FGraph)
runFunctionalGraph s = (flip runStateT) s . ml_tolist

instance (Functor m, Monad m) => GraphEngine (FunctionalGraph m) where

  type GraphEngineMonad (FunctionalGraph m) = MList (StateT FGraph m)

  type EngineVertex (FunctionalGraph m) v = LNode VertexLabel

  type EngineEdge (FunctionalGraph m) e = LEdge EdgeLabel

  type EngineVertexInsertion (FunctionalGraph m) v = VertexLabel

  type EngineEdgeInsertion (FunctionalGraph m) e = EdgeLabel

  type EngineVertexInformation (FunctionalGraph m) v = M.Map MapKey MapValue

  type EngineEdgeInformation (FunctionalGraph m) e = M.Map MapKey MapValue

  getTargetVertex proxyG proxyE proxyV (_, node, _) = do
      graph <- lift get
      case lab graph node of
          Nothing -> mzero
          Just label -> return (node, label)

  getSourceVertex proxyG proxyE proxyV (node, _, _) = do
      graph <- lift get
      case lab graph node of
          Nothing -> mzero
          Just label -> return (node, label)

  getVertices proxyG proxyV properties = do
      graph <- lift get
      let matchingVertices = ufold combine [] graph
      ml_fromlist matchingVertices
    where combine (_, n, label, _) xs =
            if matchProperties properties (vertexProperties label)
            then (n, label) : xs
            else xs

  -- I don't know if there's a more efficient way to do this. We just grab
  -- all labelled edges and check each one.
  getEdges proxyG proxyE properties = do
      graph <- lift get
      let edges = labEdges graph
      let matchingEdges = foldr combine [] edges
      ml_fromlist matchingEdges
    where
      combine e@(_, _, label) xs =
          if matchProperties properties (edgeProperties label)
          then e : xs
          else xs

  getEdgesOut proxyG proxyE proxyV properties (n, _) = do
      graph <- lift get
      -- Yes, this may error; we'll fix FGL later.
      let edges = out graph . node' . context graph $ n
      let matchingEdges = filter (\(_,_,l) -> matchProperties properties (edgeProperties l)) edges
      ml_fromlist matchingEdges

  getEdgesIn proxyG proxyE proxyV properties (n, _) = do
      graph <- lift get
      let edges = inn graph . node' . context graph $ n
      let matchingEdges = filter (\(_,_,l) -> matchProperties properties (edgeProperties l)) edges
      ml_fromlist matchingEdges

  insertVertex proxyG proxyV label = do
      graph <- lift get
      let nodes = newNodes 1 graph
      case nodes of
        [] -> mzero
        node : _ -> do
            lift $ put (insNode (node, label) graph)
            return (node, label)

  insertEdge proxyG proxyE proxySrc proxyTgt label (srcv, _) (tgtv, _) = do
      graph <- lift get
      lift $ (put (insEdge (srcv, tgtv, label) graph))
      return (srcv, tgtv, label)
