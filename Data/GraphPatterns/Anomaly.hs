{-# LANGUAGE ExistentialQuantification #-}

module Data.GraphPatterns.Anomaly (
    Anomaly(..)
  ) where

-- | Anomalies in query or mutation.
data Anomaly
  = EdgeCardinalityAnomaly
  -- ^ Declared edge cardinality not respected.
  | VertexTranslationAnomaly
  -- ^ Could not produce a Vertex from an EngineVertex. Only appears when an
  --   attempt to pull a Vertex from a DeterminesVertex instance fails, i.e.
  --   not in case of an attempt to pull a Vertex from the source or target
  --   of an Edge.
  | EdgeTranslationAnomaly
  -- ^ Could not produce an Edge from an EngineEdge. Only appears when an
  --   attempt to pull an Edge from a DeterminesEdge instance fails, i.e.
  --   not in case of an attempt to pull an Edge from a Vertex (incoming or
  --   outgoing).
  | VertexInsertionAnomaly
  -- ^ Could not insert a Vertex.
  | EdgeInsertionAnomaly
  -- ^ Could not insert an Edge.
    deriving (Show)
