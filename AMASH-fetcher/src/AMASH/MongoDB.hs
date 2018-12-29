{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module AMASH.MongoDB (AMASH.MongoDB.connect, authenticate, getAllPlugins, getAllVendors, saveNewRankings) where

import Database.MongoDB
import qualified AMASH.Config as Config
import qualified Data.Text as Text
import AMASH.Constants
import Data.Time.Clock

connect :: IO Pipe
connect = do
    ip <- Config.getIP
    port <- Config.getPort
    Database.MongoDB.connect $ Host ip port

authenticate :: Pipe -> IO Bool
authenticate pipe = do
    user <- Config.getUser
    pw <- Config.getPassword
    access pipe master "admin" $ auth user pw


-- Unpack from BSON Value
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
                ++ showInKebab application ++ "/" ++ showInKebab rankingCategory ++ "' in the DB."

selectRankings application = select ["application" =: (showApplication application)] "rankings"
pushRankings rankingCategory rankings dateTime = ["$push" =: [rankingCategoryText =: ["$position" =: 0, "$each" =: toSave]]]
                                        where rankingCategoryText = Text.pack $ showInKebab rankingCategory
                                              toSave = [["date" =: dateTime, "rankings" =: rankings]]
