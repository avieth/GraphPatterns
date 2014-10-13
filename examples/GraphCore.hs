{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module GraphCore (
    GraphCore
  ) where

import Control.Applicative (Applicative)
import Data.Void (Void)
import qualified Data.Graph as G

import Data.GraphPatterns.GraphEngine
import Data.GraphPatterns.Language

-- | The graph-core Graph system as a functor/applicative/monad.
newtype GraphCore a = GraphCore (G.Graph -> a)
  deriving (Functor, Applicative, Monad)

-- | Our GraphCore type becomes a GraphEngine!
instance GraphEngine GraphCore where
  type Graph GraphCore = G.Graph
  type VertexId GraphCore = Int
  type EdgeId GraphCore = Void
  type EdgeLabel GraphCore = ()
  type Vertex GraphCore = G.Node
  type Edge GraphCore = G.Edge
  runGraphEngine (GraphCore f) g = f g
  getVertexById vid = GraphCore $ \g -> case elem vid (G.nodes g) of
    True -> Just vid
    False -> Nothing
  -- Uh oh, graph-core edges don't have identifiers! Always return nothing.
  -- That's OK though, because no program can even call this, since
  -- EdgeId GraphCore = Void
  getEdgeById _ = GraphCore $ const Nothing
  -- graph-core edges don't have labels, so we ignore them. This is obvious from
  -- the type EdgeLabel GraphCore = ()
  getEdgesOut _ v = GraphCore $ filter (predicate v) . G.edges
    where predicate vert (G.Edge source _) = source == vert
  getEdgesIn _ v = GraphCore $ filter (predicate v) . G.edges
    where predicate vert (G.Edge _ target) = target == vert
  getTargetVertex (G.Edge _ target) = GraphCore $ const target
  getSourceVertex (G.Edge source _) = GraphCore $ const source
