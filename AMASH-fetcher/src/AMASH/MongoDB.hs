{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

-- (AMASH.MongoDB.connect, authenticate, getAllPlugins, getAllVendors, saveNewRankings)
module AMASH.MongoDB where

import qualified Data.Text as Text
import Database.MongoDB
import Data.Time.Clock

import qualified AMASH.Config as Config
import AMASH.MongoDB.Querys
import AMASH.Constants

-- | Connect to the database specified in the environment variables. Can fail due to connection errors.
connect :: IO Pipe -- ^ The pipe used to communicate with the database.
connect = do
    ip <- Config.getIP
    port <- Config.getPort
    Database.MongoDB.connect $ Host ip port

-- | Authenticate at the database using the credentials specified in the environment variables.
authenticate :: Pipe    -- ^ The pipe used to communicate with the database.
             -> IO Bool -- ^ Whether the authentication was successful.
authenticate pipe = do
    user <- Config.getUser
    pw <- Config.getPassword
    access pipe master "admin" $ auth user pw

-- | Unpack a BSON Value that holds a String into a String
unValue :: Value -> String
unValue (String text) = Text.unpack text

getAllKeys pipe collection = do
    bsonValues <- access pipe master "amash" $ find (select [] collection) >>= rest
    return $ map (unValue . valueAt "key") bsonValues

getAllPlugins pipe = getAllKeys pipe "plugins"
getAllVendors pipe = getAllKeys pipe "vendors"


saveNewRankings pipe application rankingCategory rankings = do
    currentDateTime <- getCurrentTime

    let selectDocument  = selectApplication application
        pushNewRankings = pushRankings rankingCategory rankings currentDateTime
    access pipe master "amash" $ modify selectDocument pushNewRankings

    let amountOfResults = show $ Prelude.length rankings
        applicationName = showApplication application
        rankingName     = showInKebab rankingCategory
    putStrLn $ "Persisted " ++ amountOfResults ++ " new results for '" ++ applicationName ++ "/" ++ rankingName ++ "' in the DB."
