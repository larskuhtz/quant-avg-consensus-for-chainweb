{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: QuantAvgConsensus.Dense
-- Copyright: Copyright © 2025 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module QuantAvgConsensus.Dense
( graphToAdjMatrix
, order
, degree
, randomX0
, stepN
, run
, runN
, runSteps
)
where

import Control.Monad
import Data.DiGraph qualified as G
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.Massiv.Array qualified as A
import Data.Massiv.Array.Manifest.Vector qualified as A
import Data.Massiv.Core.Operations qualified as A
import Data.Ratio
import Data.Vector qualified as V
import Prelude hiding (min, max)
import QuantAvgConsensus.Utils
import System.Random as R
import UnliftIO (pooledMapConcurrently)

-- -------------------------------------------------------------------------- --
-- Dense Adjacency Matrix Representation
-- (Note that the dense representation is faster for our graphs)

-- | The diagnal is assumed to be 1.
--
type AdjMatrix = A.Array A.P A.Ix2 Int

graphToAdjMatrix :: G.DiGraph Int -> AdjMatrix
graphToAdjMatrix g = A.makeArray A.Seq (A.Sz (n A.:. n)) go
  where
    adjs = G.adjacencySets g
    n = HM.size adjs
    go (i A.:. j)
        | i == j = 1
        | isEdge (i, j) = 1
        | otherwise = 0
    isEdge (a, b) = maybe False (HS.member b) $ HM.lookup a adjs

order :: AdjMatrix -> Int
order m = i
  where
    A.Sz (A.Ix2 i _) = A.size m
{-# INLINE order #-}

-- | The graph is assumed to be regular and reflexive.
--
degree :: AdjMatrix -> Int
degree m = A.sum $ m A.!> 0
{-# INLINE degree #-}


randomX0 :: AdjMatrix -> IO LabelingD
randomX0 m = do
    gen <- R.newStdGen
    return $ A.compute $ A.uniformRangeArray gen (0, epoch) A.Seq (A.Sz1 (order m))

-- -------------------------------------------------------------------------- --
-- Quantized Average Consensus

type LabelingD = A.Array A.P A.Ix1 Int

quotM :: A.Numeric r a => Integral a => A.Array r A.Ix1 a -> a -> A.Array r A.Ix1 a
quotM m e = A.unsafeLiftArray (`quot` e) m
{-# INLINE quotM #-}

stepN :: Int -> AdjMatrix -> LabelingD -> LabelingD
stepN n m x = foldl' (\acc _ -> go acc) x [1..n]
  where
    go :: LabelingD -> LabelingD
    go acc = A.compute $ (m A.!>< acc) `quotM` d
    d = degree m
{-# INLINE stepN #-}

run :: G.DiGraph Int -> Int -> Int -> IO (Double, Int)
run graph precision n = do
    x0 <- (A..* precision) <$> randomX0 m
    let !expected = mean (realToFrac <$> A.toList x0) / fromIntegral precision
    let !results = round @_ @Int . (% precision) <$> A.toVector (stepN n m x0)
    let !converged = maximum results - minimum results
    let !e = expected - realToFrac (V.head results)
    return (e, converged)
  where
    m = graphToAdjMatrix graph

-- -------------------------------------------------------------------------- --
-- Run N times and collect statistics

-- Precision:
--  4bytes: 2 ** 32
--  max values: 120 * 120
--
--  2 ** 32 / (120 * 120) ~ 298261

runN :: Int -> G.DiGraph Int -> Int -> Int -> IO Result
runN count g precision n = do
    results <- pooledMapConcurrently (const $ run g precision n) [1..count]
    return $ Result
        { steps = n
        , err = stats $ fst <$> results
        , convergenceCount = length $ filter ((== 0) . snd) results
        , delta = stats $ snd <$> results
        }

-- -------------------------------------------------------------------------- --
-- main

runSteps :: Int -> AdjMatrix -> LabelingD -> [(Int, Int)]
runSteps precision m x0 = go $ x0 A..* precision
  where
    go x
        | fst cur == 0 = []
        | otherwise = cur : go (A.compute $ (m A.!>< x) `quotM` d)
      where
        x' :: V.Vector Int
        !x' = round @(Ratio Int) @Int . (% precision) <$> A.toVector x
        !cur = (maximum x' - minimum x', length $ V.uniq x')

    d = degree m

-- An alternative approach would be to pick a spanning tree of the graph and
-- compute that average on that tree. This would operate in 2 phases:
-- 1. Compute the average of the tree at the root
-- 2. Propagate the average to all nodes
--
-- The disadvantage is that we would have to define a fixed tree for each graph
-- and the algorithm would need to be aware of the exact shape of that tree. The
-- advantage would be that it converges deterministically at twice the diameter
-- many steps.
--
-- Another alternative is to introduce a a new category for header validation
-- rules that has access to all blocks at a given height.

