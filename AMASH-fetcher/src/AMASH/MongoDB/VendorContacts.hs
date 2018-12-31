{-# LANGUAGE OverloadedStrings #-}

module AMASH.MongoDB.VendorContacts (persistVendorContacts) where

import AMASH.MongoDB.Querys
import AMASH.Data.Vendor.StorableVendorContact

import Database.MongoDB
import Data.Text
import Data.Time.Clock

{-
checkIfContactsAreNew pipe vendorId contacts = do
    lastSavedContacts <- access pipe master "amash" $ getLastSavedVendorContacts vendorId
    if   Prelude.length bsonDoc == 0
    then thereAreNoOlderContactsToCompareWith
    else checkIfRankingsAreNew' (lastSavedContacts !! 0) contacts

thereAreNoOlderContactsToCompareWith = do
    putStrLn "NEW DATA! - There are no earlier saved contacts to compare the newly fetched contacts with."
    return (True, Nothing)

checkIfContactsAreNew' bsonDoc rankings = do
    let maybeRankings    = bsonDoc !? "contacts" :: Maybe [Document]
        maybeDate        = bsonDoc !? "date"     :: Maybe UTCTime
        rankingsAreEqual = compareRankings maybeRankings rankings

    when (isNothing maybeDate) (error "Last saved rankings do not have a date. This means the data is corrupted!")
    putStrLn $ "The last saved data for this ranking is from '" ++ (show $ fromJust maybeDate) ++ "'."

    if   rankingsAreEqual
    then putStrLn "UNCHANGED - The last saved rankings are equal with the newly fetched rankings."
    else putStrLn "NEW DATA! - The newly fetched rankings are different from last saved rankings."

    return (rankingsAreEqual, maybeDate)
-}
persistVendorContacts :: Pipe -> Text -> [StorableVendorContact] -> IO ()
persistVendorContacts pipe vendorId contacts = do
    let docs   = Prelude.map docFromStorable contacts
        amount = Prelude.length docs

    -- TODO: check if contacts are new!!!

    currentDateTime <- getCurrentTime

    let selectDocument  = selectVendor vendorId
        pushNewContacts = pushVendorContacts currentDateTime docs
    access pipe master "amash" $ modify selectDocument pushNewContacts
    putStrLn "Persisted new vendorContacts!!"
    print docs

docFromStorable StorableVendorContact{displayName=d, userId=u} = ["displayName" =: d, "userId" =: u]
