module Main where

import Lib
import Network.Wreq
import Control.Lens
import WreqUtil
import MarketplaceURIs

main :: IO ()
main = do
    r <- get $ MarketplaceURIs.appVersionLatest "de.scandio.confluence.plugins.pocketquery"
    print $ responseGetBody r
    print $ responseIsOkay r
    print $ responseGetStatus r
