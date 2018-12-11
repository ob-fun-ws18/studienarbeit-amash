{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module MongoTest (MongoTest.connect, authenticate, getAllPlugins) where

import Database.MongoDB
import Control.Monad.Trans (liftIO)
import qualified AMASH.Config as Config
import qualified Data.Text as Text

-- creates a "run" function that accepts a DB Action (fixme: works in GHCi but won't compile when used?)
runWith pipe dbAction = access pipe master "amash" dbAction

connect = do
    ip <- Config.getIP
    port <- Config.getPort
    Database.MongoDB.connect $ Host ip port

authenticate pipe = do
    user <- Config.getUser
    pw <- Config.getPassword
    access pipe master "admin" $ auth user pw

getAllPlugins pipe = do
    pluginValues <- access pipe master "amash" $ find (select [] "plugins") >>= rest
    return $ map (unValue . valueAt "key") pluginValues

-- Unpack from BSON Value
unValue (String text) = Text.unpack text