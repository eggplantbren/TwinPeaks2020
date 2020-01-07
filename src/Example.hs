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
newtype Point =
        Point
        {
            xs :: U.Vector Double
        } deriving (Eq, Read);


instance Walkable Point where
    fromPrior  = fromPrior_
    perturb    = perturb_
    getScalars = getScalars_


-- Generate from the prior
fromPrior_ :: PrimMonad m
          => Gen (PrimState m)
          -> m Point
fromPrior_ rng = do
    xs <- U.replicateM n (uniform rng)
    return $ Point {..}


-- Perturber
perturb_ :: PrimMonad m
         => Point
         -> Gen (PrimState m)
         -> m (Point, Double)
perturb_ Point {..} rng = do
    k <- uniformR (0, n-1) rng
    (x', logH) <- boundedPerturb (xs U.! k) (0.0, 1.0) rng

    mvec <- U.thaw xs
    _ <- UM.write mvec k x'
    xs' <- U.unsafeFreeze mvec

    return (Point {xs=xs'}, logH)



getScalars_ :: Point -> (Double, Double)
getScalars_ Point {..} = (0.0, 0.0)


instance Show Point where
    show Point {..} =
        let
            renderOne x = show x ++ ","  -- renderOne :: Double -> Text
            parts       = map renderOne (U.toList xs) -- parts :: [Text]
        in
            init $ concat parts

