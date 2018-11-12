-- | A Lib module.
module Lib (square, readConfig) where

import System.Environment (getArgs)

-- | Calculate the square of a number.
square :: Num a
    => a -- ^ The number
    -> a -- ^ The square
square n = n^2

-- | Read config (plugin keys) either from args or from the file "plugins.config" if there are no args
-- | TODO: Read config from DB instead.
readConfig :: IO [String] -- ^ List of plugin keys
readConfig = do
    args <- getArgs
    if not (null args)
    then return args
    else lines <$> readFile "plugins.config"