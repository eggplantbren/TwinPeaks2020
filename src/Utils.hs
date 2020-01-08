{-# LANGUAGE NoImplicitPrelude #-} 

module Utils where

-- Imports
import BasicPrelude
import Control.Monad.Primitive
import qualified Data.Vector.Unboxed as U
import System.Random.MWC
import System.Random.MWC.Distributions


-- logsumexp of a vector
logsumexp :: U.Vector Double -> Double
logsumexp xs =
    let
        biggest = U.maximum xs
        ys      = U.map (\x -> exp $ x - biggest) xs
    in
        biggest + log(U.sum ys)


-- New year, new randh
randh :: PrimMonad m
      => Gen (PrimState m)
      -> m Double
randh rng = do

    -- Reflected exponential of scale length 3
    -- that maxes out at 1.
    e <- (\u -> 1.0 + 3.0*log u) <$> uniform rng
    let scale = 10.0**e
    (* scale) <$> standard rng



-- "Less than" for pairs
lt :: (Double, Double) -> (Double, Double) -> Bool
lt (x, y) (x', y') = (x < x') && (y < y')


-- A version that returns an Int instead of a Bool
lt' :: (Double, Double) -> (Double, Double) -> Int
lt' (x, y) (x', y') = fromEnum $ lt (x, y) (x', y')


-- Perturb a Double while staying within a given interval
boundedPerturb :: PrimMonad m
               => Double
               -> (Double, Double)
               -> Gen (PrimState m)
               -> m (Double, Double)
boundedPerturb x (left, right) rng = do
    let width = right - left
    jump <- (width*) <$> randh rng
    return (wrap (x + jump) (left, right), 0.0)



-- Mod
myMod :: Double -> Double -> Double
myMod y x = (y/x - ((fromIntegral :: Int -> Double) . floor) (y/x))*x


-- Wrap
wrap :: Double -> (Double, Double) -> Double
wrap x (a, b)
    | x < xMin || x > xMax = myMod (x - xMin) (xMax - xMin) + xMin
    | otherwise            = x
  where
    xMin = min a b
    xMax = max a b

