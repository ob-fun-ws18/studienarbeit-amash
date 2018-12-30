{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module AMASH.MongoDB where

import qualified Data.Text as Text
import Database.MongoDB
import Data.Time.Clock

import AMASH.Constants
import AMASH.MongoDB.Connection
import AMASH.MongoDB.Querys
import AMASH.MongoDB.Setup

-- Exporting functions from nested modules for single import functionality
openConnection = AMASH.MongoDB.Connection.openConnection'
authenticate   = AMASH.MongoDB.Connection.authenticate'
runSetup       = AMASH.MongoDB.Setup.runSetup'

-- | Unpack a BSON Value that holds a String into a String.
unValue :: Value -> String
unValue (String text) = Text.unpack text

getAllKeys pipe collection = do
    bsonValues <- access pipe master "amash" $ find (select [] collection) >>= rest
    return $ map (unValue . valueAt "key") bsonValues

getAllPlugins pipe = getAllKeys pipe "plugins"
getAllVendors pipe = getAllKeys pipe "vendors"

-- | Saves a new ranking for an application/category.
saveNewRankings pipe application rankingCategory rankings = do
    currentDateTime <- getCurrentTime

    let selectDocument  = selectApplication application
        pushNewRankings = pushRankings rankingCategory rankings currentDateTime
    access pipe master "amash" $ modify selectDocument pushNewRankings

    let amountOfResults = show $ Prelude.length rankings
        applicationName = showApplication application
        rankingName     = showInKebab rankingCategory
    putStrLn $ "Persisted " ++ amountOfResults ++ " new results for '" ++ applicationName ++ "/" ++ rankingName ++ "' in the DB."
