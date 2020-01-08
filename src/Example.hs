{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Example where

-- Imports
import BasicPrelude
import Control.Monad.Primitive
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import System.Random.MWC
import Utils
import Walkable

-- Number of dimensions
n :: Int
n = 20


-- A point in parameter space
newtype Example =
        Example
        {
            xs :: U.Vector Double
        } deriving (Eq, Read);


instance Walkable Example where
    fromPrior  = exampleFromPrior
    perturb    = examplePerturb
    getScalars = exampleGetScalars


-- Generate from the prior
exampleFromPrior :: PrimMonad m
                 => Gen (PrimState m)
                 -> m Example
exampleFromPrior rng = do
    xs <- U.replicateM n (uniform rng)
    return $ Example {..}


-- Perturber
examplePerturb :: PrimMonad m
               => Example
               -> Gen (PrimState m)
               -> m (Example, Double)
examplePerturb Example {..} rng = do
    k <- uniformR (0, n-1) rng
    (x', logH) <- boundedPerturb (xs U.! k) (0.0, 1.0) rng

    mvec <- U.thaw xs
    _ <- UM.write mvec k x'
    xs' <- U.unsafeFreeze mvec

    return (Example {xs=xs'}, logH)



exampleGetScalars :: Example -> (Double, Double)
exampleGetScalars Example {..} =
    let
        w = 0.01 -- Width
        tau = 1.0/w**2
        ln_f = U.foldl' (\x acc -> acc - 0.5*tau*(x - 0.5)**2) 0.0 xs
        ln_g = U.foldl' (\x acc -> acc - 0.5*tau*(x - 0.4)**2) 0.0 xs
    in
        (ln_f, ln_g)


instance Show Example where
    show Example {..} =
        let
            renderOne x = show x ++ ","  -- renderOne :: Double -> Text
            parts       = map renderOne (U.toList xs) -- parts :: [Text]
        in
            init $ concat parts

