{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module: QuantAvgConsensus.Utils
-- Copyright: Copyright © 2025 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module QuantAvgConsensus.Utils
( epoch
, Stats(..)
, stats
, mean
, Result(..)
, readGraph
) where

import Data.Aeson (ToJSON)
import Data.DiGraph qualified as G
import Data.List qualified as L
import GHC.Generics
import Prelude hiding (min, max)

-- -------------------------------------------------------------------------- --

epoch :: Int
epoch = 120 * 120

-- -------------------------------------------------------------------------- --
-- Quick and Dirty Statistics

data Stats a = Stats
    { avg :: !Double
    , min :: !a
    , max :: !a
    , median :: !a
    }
    deriving (Show, Generic, ToJSON)

stats :: Real a => [a] -> Stats a
stats xs = Stats
    { avg = mean (realToFrac <$> xs)
    , min = minimum xs
    , max = maximum xs
    , median = L.sort xs !! (length xs `div` 2)
    }

mean :: (Fractional a, Foldable t) => t a -> a
mean xs = sum xs / fromIntegral (length xs)

-- -------------------------------------------------------------------------- --
-- Result of Experiments

data Result = Result
    { steps :: !Int
    , err :: !(Stats Double)
    , convergenceCount :: !Int
    , delta :: !(Stats Int)
    }
    deriving (Show, Generic, ToJSON)

-- -------------------------------------------------------------------------- --
-- Command Line Utils

readGraph :: String -> G.DiGraph Int
readGraph g = case g of
    "d4k4" -> G.d4k4
    "d5k4" -> G.d5k4
    "twenty" -> G.twentyChainGraph
    "petersen" -> G.petersenGraph
    "d3k4" -> G.d3k4
    "d4k3" -> G.d4k3
    "d5k3" -> G.d5k3
    "hoffman" -> G.hoffmanSingleton
    _ -> error $ "unknown graph: " <> g

