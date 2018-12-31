{-# LANGUAGE OverloadedStrings #-}

module AMASH.MongoDB.Querys.Keys (
    getAllAppKeys,
    getAllVendorKeys,
    insertAppKeys,
    insertVendorKeys
) where

import AMASH.Constants

import qualified Data.Text as Text
import Control.Monad.IO.Class (MonadIO)
import Database.MongoDB

-- | Unpack a BSON Value that holds a String into a Text.
unValue :: Value -> Text.Text
unValue (String text) = text

getAllKeys pipe collection = do
    bsonValues <- access pipe master "amash" $ find (select [] collection) >>= rest
    return $ map (unValue . valueAt "key") bsonValues

getAllAppKeys :: MonadIO m => Pipe -> m [Text.Text]
getAllAppKeys    pipe = getAllKeys pipe "apps"

getAllVendorKeys :: MonadIO m => Pipe -> m [Text.Text]
getAllVendorKeys pipe = getAllKeys pipe "vendors"


insertKeys pipe database keys = do
    let docs = map (\key -> ["key" =: key]) keys
    access pipe master "amash" $ insertMany_ database docs

insertAppKeys    pipe keys = insertKeys pipe "apps" keys
insertVendorKeys pipe keys = insertKeys pipe "vendors" keys