{-# LANGUAGE OverloadedStrings #-}

module AMASH.MongoDB.App (persistAppMetrics, persistAppMetadata) where

import AMASH.MongoDB.Querys
import AMASH.MongoDB.Helpers
import qualified AMASH.Data.AppMetrics as AppMetrics
import qualified AMASH.Data.Vendor as Vendor
import qualified AMASH.Data.StorableApp as StorableApp

import GenericBson
import Database.MongoDB
import Data.Text
import Data.Time.Clock
import Data.Maybe

persistAppMetrics :: Pipe -> Text -> AppMetrics.AppMetrics -> IO ()
persistAppMetrics = persistAppData "metrics"

persistAppMetadata :: Pipe -> Text -> StorableApp.StorableApp -> IO ()
persistAppMetadata = persistAppData "metadata"


persistAppData fetchedName pipe appKey fetchedData = do
    let fetchedBson         = toBSON fetchedData
        getLastSavedAction  = access pipe master "amash" $ getLastSavedAppEntry fetchedName appKey
        compareWithOldData  = compareFetchedAndOldData fetchedData fetchedName

    checkResult <- getLastSavedDataAndCompare pipe getLastSavedAction compareWithOldData

    let areEqual      = fst checkResult
        maybeObjectId = snd checkResult :: Maybe ObjectId

    if   areEqual && isJust maybeObjectId
    then updateLastChangedTimestamp pipe (fromJust maybeObjectId :: ObjectId) (unpack fetchedName)
    else putNewEntryIntoDatabase pipe appKey fetchedBson (unpack fetchedName)

putNewEntryIntoDatabase :: Pipe -> Text -> Document -> String -> IO ()
putNewEntryIntoDatabase pipe appKey fetchedBson fetchedName = do
    currentDateTime <- getCurrentTime
    let docToPersist = [ "fetched"        =: currentDateTime
                       , "lastChecked"    =: currentDateTime
                       , "app"            =: appKey
                       , pack fetchedName =: fetchedBson]
    access pipe master "amash" $ insert_ (pack $ "app-" ++ fetchedName) docToPersist
    putStrLn $ "Saved new " ++ fetchedName ++ " in the database."

updateLastChangedTimestamp :: Pipe -> ObjectId -> String -> IO ()
updateLastChangedTimestamp pipe objectId fetchedName = do
    currentDateTime <- getCurrentTime

    let selectDocument    = selectByObjectId (pack $ "app-" ++ fetchedName) objectId
        updateLastChecked = ["$set" =: ["lastChecked" =: currentDateTime]]

    access pipe master "amash" $ modify selectDocument updateLastChecked
    putStrLn $ "Updated lastChecked of old entry to '" ++ show currentDateTime ++ "'."

