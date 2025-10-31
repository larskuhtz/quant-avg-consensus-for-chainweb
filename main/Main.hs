{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Main
-- Copyright: Copyright © 2025 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Main
( main
) where

import Control.Monad
import Data.Aeson (encode)
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.DiGraph qualified as G
import Prelude hiding (min, max)
import System.Environment
import System.Console.GetOpt
import QuantAvgConsensus.Utils
import QuantAvgConsensus.Dense

data Opts = Opts
    { _optsGraph :: !(G.DiGraph Int)
    , _optsSampleCount :: !Int
    , _optsPrecision :: !Int
    , _optsStepCount :: !Int
    }
    deriving Show

options :: [OptDescr (Opts -> Opts)]
options =
    [ Option ['g'] ["graph"]
        (ReqArg (\str opts -> opts {_optsGraph =  readGraph str}) "GRAPH")
        "graph"
    , Option ['s'] ["samples"]
        (ReqArg (\str opts -> opts {_optsSampleCount  = read str}) "SAMPLES")
        "Number of samples"
    , Option ['p'] ["precision"]
        (ReqArg (\str opts -> opts {_optsPrecision = read str}) "PRECISION")
        "Qunatization / Precision"
    , Option ['c'] ["steps"]
        (ReqArg (\str opts -> opts {_optsStepCount = read str}) "STEPS")
        "Number of steps"
    ]

main :: IO ()
main = do
    (opts_, args, errs) <- getOpt Permute options <$> getArgs
    case errs of
        [] -> return ()
        _ -> error $ concat errs <> "\n" <> usageInfo header options
    case args of
        ["samples"] ->
            mainSamples $ foldl (flip id) (Opts G.d4k4 100000 1000 120) opts_
        ["stats"] ->
            mainStats $ foldl (flip id) (Opts G.d4k4 1000 1000 120) opts_
        ["cases"] ->
            mainCases $ foldl (flip id) (Opts G.d4k4 10000 1000 120) opts_
        e -> error $ "Unknown command or arguments: " <> show e
            <> "\n" <> usageInfo header options
  where
    header = "Usage: quant-avg-consensus COMMAND [OPTIONS]\nCommands: samples|stats|cases\nOptions:"

mainSamples :: Opts -> IO ()
mainSamples opts = do
    let m = graphToAdjMatrix (_optsGraph opts)
    replicateM_ (_optsSampleCount opts) $ do
        x0 <- randomX0 m
        let !r = runSteps (_optsPrecision opts) m x0
        forM_ (zip [0..] r) $ \(a,b) -> putStrLn
            $ show @Int a
            <> "," <> show (fst b)
            <> "," <> show (snd b)

mainStats :: Opts -> IO ()
mainStats opts = do
    r <- runN
        (_optsSampleCount opts)
        (_optsGraph opts)
        (_optsPrecision opts)
        (_optsStepCount opts)
    BL8.putStrLn $ encode r

mainCases :: Opts -> IO ()
mainCases opts = do
    let results = runCases
            (_optsGraph opts)
            (_optsPrecision opts)
            (_optsStepCount opts)
    BL8.putStrLn $ encode results
