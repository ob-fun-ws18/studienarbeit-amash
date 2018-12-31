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
    let docs               = Prelude.map toBSON fetchedContacts
        getLastSavedAction = access pipe master "amash" $ getLastSavedVendorContacts vendorId
        compareWithOldData = compareFetchedAndOldData fetchedContacts "contacts"

    checkResult <- getLastSavedDataAndCompare pipe getLastSavedAction fetchedContacts compareWithOldData

    let areEqual            = fst checkResult
        maybeUnchangedSince = snd checkResult :: Maybe UTCTime

    if   areEqual && isJust maybeUnchangedSince
    then putUnchangedSinceIntoDatabase pipe vendorId maybeUnchangedSince
    else putNewContactsIntoDatabase pipe vendorId docs

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
    putStrLn "Persisted 'unchanged since' pointer in the DB."
