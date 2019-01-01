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
persistVendorMetaData pipe vendorId vendorMetaData = do
    let doc                = toBSON vendorMetaData
        getLastSavedAction = access pipe master "amash" $ getLastSavedVendorContacts vendorId
        compareWithOldData = compareFetchedAndOldData vendorMetaData "metadata"

    checkResult <- getLastSavedDataAndCompare pipe getLastSavedAction compareWithOldData

    let areEqual            = fst checkResult
        --maybeUnchangedSince = snd checkResult :: Maybe UTCTime
        maybeUnchangedSince = Nothing :: Maybe UTCTime

    if   areEqual && isJust maybeUnchangedSince
    then putUnchangedSinceIntoDatabase pipe vendorId maybeUnchangedSince
    else putNewContactsIntoDatabase pipe vendorId doc

putNewContactsIntoDatabase pipe vendorId docs = do
    currentDateTime <- getCurrentTime
    let selectDocument  = selectVendor vendorId
        pushNewContacts = pushVendorContacts currentDateTime docs
    access pipe master "amash" $ modify selectDocument pushNewContacts
    putStrLn "Saved new contacts in the database."

putUnchangedSinceIntoDatabase pipe vendorId maybeUnchangedSince = do
    currentDateTime <- getCurrentTime
    let selectDocument  = selectVendor vendorId
        pushNewData     = pushUnchangedSinceVendorContacts currentDateTime (fromJust maybeUnchangedSince)
    access pipe master "amash" $ modify selectDocument pushNewData
    putStrLn "Persisted 'unchanged since' pointer in the database."
