{-# LANGUAGE RecordWildCards #-}

module SamplerState where

-- Imports
import Control.Monad.Primitive
import qualified Data.Vector as V
import System.Random.MWC
import Walkable


-- Sampler type
data SamplerState a =
    SamplerState
    {
        particles :: !(V.Vector a),
        scalars   :: !(V.Vector (Double, Double))
    } deriving (Eq, Read, Show);


-- Generate an initial sampler state of the given size.
genSamplerState :: (PrimMonad m, Walkable a)
                => Int
                -> Gen (PrimState m)
                -> m (SamplerState a)
genSamplerState numParticles rng = do
    particles <- V.replicateM numParticles (fromPrior rng)
    let scalars = V.map getScalars particles
    return $ SamplerState {..}

