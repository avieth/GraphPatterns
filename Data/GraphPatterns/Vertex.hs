{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Data.GraphPatterns.Vertex (
    Vertex(..)
  , DeterminesVertex(..)
  ) where

import Data.Proxy

import Data.GraphPatterns.GraphEngine

class GraphEngine m => Vertex m v where
  toEngineVertex :: Proxy m -> v -> EngineVertex m
  -- ^ We give the first parameter so that we can disambiguate.
  fromEngineVertex :: Proxy m -> EngineVertex m -> Maybe v

-- | Can't do a default reflexive instance because we don't know if a given
--   Vertex is unique.
class (GraphEngine m, Vertex m v) => DeterminesVertex m determiner v where
  -- One of Unique or NotUnique
  type VertexUniqueness m determiner v :: *
  toEngineVertexInformation
    :: Proxy m
    -> Proxy v
    -> determiner
    -> EngineVertexInformation m
