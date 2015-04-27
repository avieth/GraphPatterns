{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Data.GraphPatterns.Vertex (
    Vertex(..)
  , DeterminesVertex(..)
  ) where

import Data.Proxy
import Data.GraphPatterns.GraphEngine

class GraphEngine m => Vertex m v where
  toEngineVertexInsertion :: v -> EngineVertexInsertion m v
  fromEngineVertex :: EngineVertex m v -> (Effect m) ((Result m) v)
  -- ^ A way to go from an EngineVertex m v to a "proper" value of the user
  --   defined type. We give no v -> EngineVertex m v because the GraphEngine
  --   is the only party licenced to create EngineVertex m v instances.

-- | Can't do a default reflexive instance because we don't know if a given
--   Vertex is unique, nor do we know how to dump it to EngineVertexInformation.
class Vertex m v => DeterminesVertex m v determiner where
  -- One of Unique or NotUnique
  type VertexUniqueness m v determiner :: *
  toEngineVertexInformation
    :: determiner
    -> EngineVertexInformation m v
