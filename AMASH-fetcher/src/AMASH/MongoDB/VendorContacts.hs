{-# LANGUAGE OverloadedStrings #-}

module AMASH.MongoDB.VendorContacts (persistVendorContacts) where

import AMASH.MongoDB.Querys
import AMASH.MongoDB.Helpers
import AMASH.Data.Vendor.StorableVendorContact

import GenericBson
import Database.MongoDB
import Data.Text
import Data.Time.Clock
import Data.Maybe

persistVendorContacts :: Pipe -> Text -> [StorableVendorContact] -> IO ()
persistVendorContacts pipe vendorId fetchedContacts = do
    let fetchedContactsBson = Prelude.map toBSON fetchedContacts
        getLastSavedAction  = access pipe master "amash" $ getLastSavedVendorContacts vendorId
        compareWithOldData  = compareFetchedAndOldData fetchedContacts "contacts"

    checkResult <- getLastSavedDataAndCompare pipe getLastSavedAction compareWithOldData

    let areEqual      = fst checkResult
        maybeObjectId = snd checkResult :: Maybe ObjectId

    if   areEqual && isJust maybeObjectId
    then updateLastChangedTimestamp pipe (fromJust maybeObjectId :: ObjectId)
    else putNewContactsIntoDatabase pipe vendorId fetchedContactsBson

putNewContactsIntoDatabase pipe vendorId contacts = do
    currentDateTime <- getCurrentTime
    let docToPersist = [ "fetched"     =: currentDateTime
                       , "lastChecked" =: currentDateTime
                       , "vendor"      =: vendorId
                       , "contacts"    =: contacts]
    access pipe master "amash" $ insert_ "vendor-contacts" docToPersist
    putStrLn "Saved new contacts in the database."

updateLastChangedTimestamp pipe objectId = do
    currentDateTime <- getCurrentTime

    let selectDocument    = selectByObjectId "vendor-contacts" objectId
        updateLastChecked = ["$set" =: ["lastChecked" =: currentDateTime]]

    access pipe master "amash" $ modify selectDocument updateLastChecked
    putStrLn $ "Updated lastChecked of old entry to '" ++ (show currentDateTime) ++ "'."
