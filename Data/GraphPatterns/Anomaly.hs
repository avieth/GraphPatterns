{-# LANGUAGE ExistentialQuantification #-}

module Data.GraphPatterns.Anomaly (
    Anomaly(..)
  ) where

data Anomaly
  = EdgeCardinalityAnomaly
  | VertexTranslationAnomaly
  | EdgeTranslationAnomaly
  | VertexDeterminationAnomaly
  | EdgeDeterminationAnomaly
    deriving (Show)
