{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: QuantAvgConsensus.Sparse
-- Copyright: Copyright © 2025 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module QuantAvgConsensus.Sparse
( Graph(..)
, digraphToGraph
, order
, adjacents
, degree
, Labeling
, randomX0
, run
, runN
, sparseMain
) where

import Control.Monad
import Data.DiGraph qualified as G
import Data.HashSet qualified as HS
import Data.Ratio
import Data.Vector qualified as V
import Prelude hiding (min, max)
import QuantAvgConsensus.Utils
import System.Random as R
import UnliftIO (pooledMapConcurrently)
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Aeson (encode)
import System.Environment (getArgs)

-- -------------------------------------------------------------------------- --
-- Sparse Adjacency List Representation (Slower for our graphs)

newtype Graph = Graph { _getGraph :: V.Vector (V.Vector Int) }

digraphToGraph :: G.DiGraph Int -> Graph
digraphToGraph g = Graph $ V.generate n $ \i ->
    V.fromList $ HS.toList $ G.adjacents i g
  where
    n = fromIntegral $ G.order g

order :: Graph -> Int
order = V.length . _getGraph
{-# INLINE order #-}

adjacents :: Int -> Graph -> V.Vector Int
adjacents k (Graph g) = g V.! k
{-# INLINE adjacents #-}

degree :: Graph -> Int
degree g = V.length (adjacents 0 g)
{-# INLINE degree #-}

type Labeling = V.Vector Int

-- Random labeling (vote counts)
randomX0 :: Graph -> IO Labeling
randomX0 g = V.unfoldrExactN (order g) (R.uniformR (0, epoch)) <$> R.newStdGen
{-# INLINE randomX0 #-}

-- Step function: compute average form all parents
step :: Graph -> Labeling -> Labeling
step g x = V.generate (order g) $ \i  -> ceiling
    $ (% (1 + d))
    $ (+) (x V.! i)
    $ V.sum
    $ V.map (x V.!)
    $ adjacents i g
  where
    d = degree g

-- run with fixed point precision
--
run :: Graph -> Int -> Int -> IO (Double, Int)
run g precision n = do
    x0 <- fmap (* precision) <$> randomX0 g
    let !expected = mean (realToFrac <$> x0) / fromIntegral precision
    let !results = round @_ @Int . (% precision)
            <$> foldl' (\acc _ -> step g acc) x0 [0..n-1]
    let !converged = maximum results - minimum results
    let !e = expected - realToFrac (results V.! 0)
    return (e, converged)

runN :: Int -> Graph -> Int -> Int -> IO Result
runN count g precision n = do
    results <- pooledMapConcurrently (const $ run g precision n) [1..count]
    return $ Result
        { steps = n
        , err = stats $ fst <$> results
        , convergenceCount = length $ filter ((== 0) . snd) results
        , delta = stats $ snd <$> results
        }

sparseMain :: IO ()
sparseMain = do
    [graph, samples, precision, stepcount] <- args
    let g = digraphToGraph $ readGraph graph
    r <- runN (read samples) g (read precision) (read stepcount)
    BL8.putStrLn $ encode r
  where
    defaults = ["d4k4", "1000", "1000", "120"]
    args = zipWith (\a b -> if null a then b else a)
        <$> ((++ ["", "", "", ""]) <$> getArgs)
        <*> pure defaults
