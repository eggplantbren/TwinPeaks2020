module Utils where

-- Imports
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
    e <- (\u -> 1.0 + 3.0*log(u)) <$> uniform rng
    let scale = 10.0**e
    (* scale) <$> standard rng

