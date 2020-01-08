{-# LANGUAGE NoImplicitPrelude #-} 

module Main where

-- Imports
import BasicPrelude
import Example
import System.Random.MWC
import Walkable

main :: IO ()
main = withSystemRandom . asGenIO $ \rng -> do
    particle <- fromPrior rng :: IO Example
    explore particle 1000000 100 rng

