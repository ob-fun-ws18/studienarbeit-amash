module Main (main) where

import Criterion.Main (bench, bgroup, defaultMain, nf)
import Lib

main :: IO ()
main = defaultMain
    [
        bgroup "Lib" [
            bench "1" $ nf square (1 :: Int),
            bench "200" $ nf square (200 :: Integer),
            bench "200.0" $ nf square (200.0 :: Double),
            bench "20.8" $ nf square (20.8 :: Float)
        ]
    ]

