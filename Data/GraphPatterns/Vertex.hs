{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Data.GraphPatterns.Vertex (

    Vertex(..)
  , DeterminesVertex(..)

  ) where

import Data.Proxy
import Data.GraphPatterns.GraphEngine

-- | A Haskell datatype can be treated as a Vertex in a given GraphEngine.
class GraphEngine g => Vertex g v where
  toEngineVertexInsertion :: Proxy g -> v -> EngineVertexInsertion g v
  fromEngineVertex :: Proxy g -> Proxy v -> EngineVertex g v -> GraphEngineMonad g v

-- | Can't do a default reflexive instance because we don't know if a given
--   Vertex is unique, nor do we know how to dump it to EngineVertexInformation.
class Vertex g v => DeterminesVertex g v determiner where
  toEngineVertexInformation
    :: Proxy g
    -> Proxy v
    -> determiner
    -> EngineVertexInformation g v
