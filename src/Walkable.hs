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

    -- Render to text value for CSV output
    render :: a -> T.Text

