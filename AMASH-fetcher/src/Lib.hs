-- | A Lib module.
module Lib (helloWorld, square) where

-- | Basic Hello World function.
helloWorld :: IO ()
helloWorld = putStrLn "Hello, World!"

-- | Calculate the square of a number
square :: Num a
    => a -- ^ The number
    -> a -- ^ The square
square n = n^2
