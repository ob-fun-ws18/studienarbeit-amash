{-# LANGUAGE OverloadedStrings #-}

module AMASH.MongoDB.Querys.TrackedKeys (
    getAllTrackedAppKeys,
    getAllTrackedVendorKeys,
    insertTrackedAppKeys,
    insertTrackedVendorKeys
) where

import AMASH.Constants

import qualified Data.Text as Text
import Control.Monad.IO.Class (MonadIO)
import Database.MongoDB

-- | Unpack a BSON Value that holds a String into a Text.
unValue :: Value -> Text.Text
unValue (String text) = text

getAllTrackedKeys pipe collection = do
    bsonValues <- access pipe master "amash" $ find (select [] collection) >>= rest
    return $ map (unValue . valueAt "key") bsonValues

getAllTrackedAppKeys :: MonadIO m => Pipe -> m [Text.Text]
getAllTrackedAppKeys pipe = getAllTrackedKeys pipe "tracked-apps"

getAllTrackedVendorKeys :: MonadIO m => Pipe -> m [Text.Text]
getAllTrackedVendorKeys pipe = getAllTrackedKeys pipe "tracked-vendors"


insertTrackedKeys pipe database keys = do
    let docs = map (\key -> ["key" =: key]) keys
    access pipe master "amash" $ insertMany_ database docs

insertTrackedAppKeys    pipe keys = insertTrackedKeys pipe "tracked-apps" keys
insertTrackedVendorKeys pipe keys = insertTrackedKeys pipe "tracked-vendors" keys