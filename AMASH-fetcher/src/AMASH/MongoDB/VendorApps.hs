{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module AMASH.MongoDB.VendorApps (persistVendorApps, persistVendorArchivedApps) where

import qualified Data.Text as Text
import Database.MongoDB
import Data.Time.Clock
import Data.Maybe
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO)
import GenericBson

import AMASH.Util
import AMASH.Constants
import AMASH.MongoDB.Connection
import AMASH.MongoDB.Querys
import AMASH.MongoDB.Setup
import AMASH.MongoDB.Helpers

persistVendorApps         pipe = persistVendorApps' pipe "vendor-apps"
persistVendorArchivedApps pipe = persistVendorApps' pipe "vendor-archived-apps"

persistVendorApps' :: Pipe -> Text.Text -> Text.Text -> [Text.Text] -> IO ()
persistVendorApps' pipe collectionName vendorId fetchedData = do
    let getLastSavedAction   = access pipe master "amash" $ getLastSavedVendorEntry collectionName vendorId
        compareWithOldDataFn = compareFetchedAndOldData fetchedData "apps"

    checkResult <- getLastSavedDataAndCompare pipe getLastSavedAction compareWithOldDataFn

    let areEqual      = fst checkResult
        maybeObjectId = snd checkResult :: Maybe ObjectId

    if   areEqual && isJust maybeObjectId
    then updateLastChangedTimestamp pipe collectionName (fromJust maybeObjectId :: ObjectId)
    else putNewDataIntoDatabase pipe collectionName vendorId fetchedData

putNewDataIntoDatabase pipe collectionName vendorId rankings = do
    currentDateTime <- getCurrentTime
    let docToPersist = [ "fetched"     =: currentDateTime
                       , "lastChecked" =: currentDateTime
                       , "apps"        =: rankings
                       , "vendor"      =: vendorId]
    access pipe master "amash" $ insert_ collectionName docToPersist
    putStrLn "Saved new entry in the database."

updateLastChangedTimestamp pipe collectionName objectId = do
    currentDateTime <- getCurrentTime

    let selectDocument    = selectByObjectId collectionName objectId
        updateLastChecked = ["$set" =: ["lastChecked" =: currentDateTime]]

    access pipe master "amash" $ modify selectDocument updateLastChecked
    putStrLn $ "Updated lastChecked of old entry to '" ++ show currentDateTime ++ "'."

