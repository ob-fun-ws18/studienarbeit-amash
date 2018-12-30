{-# LANGUAGE OverloadedStrings #-}

module AMASH.MongoDB where

import qualified Data.Text as Text
import Database.MongoDB

import AMASH.MongoDB.Connection
import AMASH.MongoDB.Querys
import AMASH.MongoDB.Setup
import AMASH.MongoDB.Rankings

-- Exporting functions from nested modules for single import functionality
openConnection  = AMASH.MongoDB.Connection.openConnection'
authenticate    = AMASH.MongoDB.Connection.authenticate'
runSetup        = AMASH.MongoDB.Setup.runSetup'
saveNewRankings = AMASH.MongoDB.Rankings.saveNewRankings'

-- | Unpack a BSON Value that holds a String into a String.
-- TODO: remove, das hier kommmt nur daher, dass ich vorher valueAt nicht verstanden habe (typ angeben!)
unValue :: Value -> String
unValue (String text) = Text.unpack text

getAllKeys pipe collection = do
    bsonValues <- access pipe master "amash" $ find (select [] collection) >>= rest
    return $ map (unValue . valueAt "key") bsonValues

getAllPlugins pipe = getAllKeys pipe "plugins"
getAllVendors pipe = getAllKeys pipe "vendors"