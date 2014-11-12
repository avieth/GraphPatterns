{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Data.GraphPatterns.Vertex (
    Vertex(..)
  , DeterminesVertex(..)
  ) where

import Data.Proxy

import Data.GraphPatterns.GraphEngine

class GraphEngine m => Vertex m v where
  toEngineVertexInsertion :: v -> EngineVertexInsertion m v
  fromEngineVertex :: EngineVertex m v -> Maybe v
  -- ^ A way to go from an EngineVertex m v to a "proper" value of the user
  --   defined type. We give no v -> EngineVertex m v because the GraphEngine
  --   is the only party licenced to create EngineVertex m v instances.
  --
  -- There's an analogous function in the Edge class. I think it would be cool
  -- if we could transparently treat the result of vertex inside GraphQueries
  -- as a proper value and as an EngineVertex, so that whenever the user tries
  -- to inspect it, the pure value is computed from the EngineVertex
  -- automagically, but that's not essential.

-- | Can't do a default reflexive instance because we don't know if a given
--   Vertex is unique, nor do we know how to dump it to EngineVertexInformation.
class Vertex m v => DeterminesVertex m determiner v where
  -- One of Unique or NotUnique
  type VertexUniqueness m determiner v :: *
  toEngineVertexInformation
    :: Proxy m
    -> Proxy v
    -> determiner
    -> EngineVertexInformation m v
