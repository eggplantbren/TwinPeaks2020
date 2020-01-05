module Utils where

-- Imports
import qualified Data.Vector.Unboxed as U


-- logsumexp of a vector
logsumexp :: U.Vector Double -> Double
logsumexp xs =
    let
        biggest = U.maximum xs
        ys      = U.map (\x -> exp $ x - biggest) xs
    in
        biggest + log(U.sum ys)

