{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

-- (AMASH.MongoDB.connect, authenticate, getAllPlugins, getAllVendors, saveNewRankings)
module AMASH.MongoDB where

import Database.MongoDB
import qualified AMASH.Config as Config
import qualified Data.Text as Text
import AMASH.Constants
import Data.Time.Clock


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
    let applicationDocument = selectRankings application
        pushRankingsAction = pushRankings rankingCategory rankings currentDateTime
    access pipe master "amash" $ modify applicationDocument pushRankingsAction
    putStrLn $ "Persisted " ++ (show $ Prelude.length rankings) ++ " new results for '"
                ++ showApplication application ++ "/" ++ showInKebab rankingCategory ++ "' in the DB."

selectRankings application = select ["application" =: (showApplication application)] "rankings"
pushRankings rankingCategory rankings dateTime = ["$push" =: [rankingCategoryText =: ["$position" =: 0, "$each" =: toSave]]]
                                        where rankingCategoryText = Text.pack $ showInKebab rankingCategory
                                              toSave = [["date" =: dateTime, "rankings" =: rankings]]

getLatestRanking application rankingCategory = aggregate "rankings" [
        ["$match"   =: [
            "application" =: appName
        ]],
        ["$unwind"  =: arrayName],
        ["$limit"   =: 1],
        ["$project" =: [
            "date"     =: arrayName ++ ".date",
            "rankings" =: arrayName ++ ".rankings"
        ]]
    ] where arrayName = "$" ++ showInKebab rankingCategory
            appName   = showApplication application
