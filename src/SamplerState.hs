{-# LANGUAGE NoImplicitPrelude #-} 
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module SamplerState where

-- Imports
import BasicPrelude
import Control.Monad.Primitive
import qualified Data.Vector as V
import System.Random.MWC
import Utils
import Walkable


-- Sampler type
data SamplerState a =
    SamplerState
    {
        particles :: !(V.Vector a),
        scalars   :: !(V.Vector (Double, Double))
    } deriving (Eq, Read);


-- Generate an initial sampler state of the given size.
genSamplerState :: (PrimMonad m, Walkable a)
                => Int
                -> Gen (PrimState m)
                -> m (SamplerState a)
genSamplerState numParticles rng = do
    particles <- V.replicateM numParticles (fromPrior rng)
    let scalars = V.map getScalars particles
    return $ SamplerState {..}




-- Calculate a single lower corner count
lcc :: (Double, Double) -> SamplerState a -> Int
lcc pair SamplerState {..} =
    let
        shadowed = V.map (`lt'` pair) scalars
    in
        V.sum shadowed



-- LCCs of all particles
particleLccs :: SamplerState a -> V.Vector Int
particleLccs SamplerState {..} = V.map lcc' scalars 
    where
        lcc' s = lcc s SamplerState {..}


-- Particles with zero LCCs
zeroLccParticles :: SamplerState a -> V.Vector Bool
zeroLccParticles = V.map (== 0) . particleLccs


-- Select a zero LCC particle and return its index.
chooseParticle :: PrimMonad m
               => SamplerState a
               -> Gen (PrimState m)
               -> m Int
chooseParticle SamplerState {..} rng = do
    let candidates = zeroLccParticles SamplerState {..}
    let randIndex bools rng' = do
            i <- uniformR (0, V.length bools - 1) rng'
            if bools V.! i then return i else randIndex bools rng'
    randIndex candidates rng

-- Show instance
instance Show (SamplerState a) where
    show SamplerState {..} =
        let
            lccs = particleLccs SamplerState {..}
            rows = V.zipWith (\(a, b) c -> (a, b, c)) scalars lccs
            pr (a, b, c) = show a ++ "," ++ show b ++ "," ++ show c ++ "\n"
        in
            concat $ V.toList (V.map pr rows)

