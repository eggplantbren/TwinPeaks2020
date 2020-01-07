{- Defining a typeclass for types that can be sampled. -}

module Walkable where

-- Imports
import Control.Monad.Primitive
import qualified Data.Text as T
import System.Random.MWC

-- Functions that must be defined for a sampleable type
class Walkable a where

    -- Generate a point from the prior
    fromPrior :: PrimMonad m
              => Gen (PrimState m) -> m a

    -- Metropolis proposal that returns proposed value and logH
    perturb :: PrimMonad m
            => a -> Gen (PrimState m) -> m (a, Double)

    -- The two scalars
    getScalars :: a -> (Double, Double)

    -- Render to text value for CSV output
    render :: a -> T.Text



-- Do a Metropolis move wrt the prior
-- TODO: Implement
-- TODO: Allow constraints
metropolis :: (PrimMonad m, Walkable a)
           => a
           -> Gen (PrimState m)
           -> m a
metropolis x rng = do
    (proposal, logH) <- perturb x rng
    let logH' = if logH > 0.0 then 0.0 else logH
    u <- uniformR (0.0 :: Double, 1.0 :: Double) rng
    let x' = if u < (exp logH') then proposal else x
    return x'

