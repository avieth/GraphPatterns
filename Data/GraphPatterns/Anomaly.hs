{-# LANGUAGE ExistentialQuantification #-}

module Data.GraphPatterns.Anomaly (
    Anomaly(..)
  , Anomalized
  ) where

type Anomalized = Either Anomaly

data Anomaly
  = EdgeCardinalityAnomaly
  | VertexTranslationAnomaly
  | EdgeTranslationAnomaly
  | VertexDeterminationAnomaly
  | EdgeDeterminationAnomaly
