{- Defining a typeclass for types that can be sampled. -}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Walkable where

-- Imports
import BasicPrelude
import Control.Monad.Primitive
import System.Random.MWC

-- Functions that must be defined for a sampleable type
class (Show a) => Walkable a where

    -- Generate a point from the prior
    fromPrior :: PrimMonad m
              => Gen (PrimState m) -> m a

    -- Metropolis proposal that returns proposed value and logH
    perturb :: PrimMonad m
            => a -> Gen (PrimState m) -> m (a, Double)

    -- The two scalars
    getScalars :: a -> (Double, Double)


-- Do a Metropolis move wrt the prior
-- TODO: Allow constraints
metropolisStep :: (PrimMonad m, Walkable a)
               => a
               -> Gen (PrimState m)
               -> m a
metropolisStep x rng = do
    (proposal, logH) <- perturb x rng
    let logH' = if logH > 0.0 then 0.0 else logH
    u <- uniformR (0.0 :: Double, 1.0 :: Double) rng
    let x' = if u < exp logH' then proposal else x
    return x'


-- Do metropolis steps with logging
explore_ :: Walkable a
        => a
        -> Int
        -> Int
        -> Int
        -> Gen RealWorld
        -> IO ()
explore_ x i steps thin rng = do
    let output k =  when (i `mod` thin == 0) (putStrLn $ tshow k ++ "," ++ tshow x)
    output i
    x' <- metropolisStep x rng
    if i >= steps-1 then output (i+1) else explore_ x' (i+1) steps thin rng


-- Version with i starting from zero. This is the one that should be used.
explore :: Walkable a
        => a
        -> Int
        -> Int
        -> Gen RealWorld
        -> IO ()
explore x steps thin rng = explore_ x 0 steps thin rng

