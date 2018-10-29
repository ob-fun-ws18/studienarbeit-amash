-- | A Lib module.
module Lib (square) where

-- | Calculate the square of a number.
square :: Num a
    => a -- ^ The number
    -> a -- ^ The square
square n = n^2