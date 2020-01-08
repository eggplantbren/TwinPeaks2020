{-# LANGUAGE NoImplicitPrelude #-} 

module Main where

-- Imports
import BasicPrelude
import Example
import SamplerState
import System.Random.MWC
import Walkable


-- Generate a sampler state and print it to stdout
main :: IO ()
main = withSystemRandom . asGenIO $ \rng -> do
    samplerState <- genSamplerState 1000 rng :: IO (SamplerState Example)
    print samplerState 


-- Test Metropolis engine
metropolis :: IO ()
metropolis = withSystemRandom . asGenIO $ \rng -> do
    particle <- fromPrior rng :: IO Example
    explore particle 100000 10 rng

