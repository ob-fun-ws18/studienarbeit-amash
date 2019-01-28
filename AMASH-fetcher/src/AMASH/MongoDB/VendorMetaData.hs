{-# LANGUAGE OverloadedStrings #-}

module AMASH.MongoDB.VendorMetaData (persistVendorMetaData) where

import AMASH.MongoDB.Querys
import AMASH.MongoDB.Helpers
import AMASH.Data.Vendor

import GenericBson
import Database.MongoDB
import Data.Text
import Data.Time.Clock
import Data.Maybe

persistVendorMetaData :: Pipe -> Text -> Vendor -> IO ()
persistVendorMetaData pipe vendorId fetchedVendor = do
    let fetchedBson         = toBSON fetchedVendor
        getLastSavedAction  = access pipe master "amash" $ getLastSavedVendorMetaData vendorId
        compareWithOldData  = compareFetchedAndOldData fetchedVendor "metadata"

    checkResult <- getLastSavedDataAndCompare pipe getLastSavedAction compareWithOldData

    let areEqual      = fst checkResult
        maybeObjectId = snd checkResult :: Maybe ObjectId

    if   areEqual && isJust maybeObjectId
    then updateLastChangedTimestamp pipe (fromJust maybeObjectId :: ObjectId)
    else putNewEntryIntoDatabase pipe vendorId fetchedBson

putNewEntryIntoDatabase pipe vendorId contacts = do
    currentDateTime <- getCurrentTime
    let docToPersist = [ "fetched"     =: currentDateTime
                       , "lastChecked" =: currentDateTime
                       , "vendor"      =: vendorId
                       , "metadata"    =: contacts]
    access pipe master "amash" $ insert_ "vendor-metadata" docToPersist
    putStrLn "Saved new metadata in the database."

updateLastChangedTimestamp pipe objectId = do
    currentDateTime <- getCurrentTime

    let selectDocument    = selectByObjectId "vendor-metadata" objectId
        updateLastChecked = ["$set" =: ["lastChecked" =: currentDateTime]]

    access pipe master "amash" $ modify selectDocument updateLastChecked
    putStrLn $ "Updated lastChecked of old entry to '" ++ show currentDateTime ++ "'."
