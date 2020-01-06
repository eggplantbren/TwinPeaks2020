module Main where

-- Imports
import qualified Data.Text.IO as T
import Example
import System.Random.MWC
import Walkable

main :: IO ()
main = withSystemRandom . asGenIO $ \rng -> do
    particle <- fromPrior rng :: IO Point
    T.putStrLn $ render particle

