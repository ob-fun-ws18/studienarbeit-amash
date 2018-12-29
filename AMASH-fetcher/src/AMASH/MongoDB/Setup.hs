{-# LANGUAGE OverloadedStrings #-}

module AMASH.MongoDB.Setup (runSetup) where

import Database.MongoDB
import Control.Monad.IO.Class
import AMASH.REST.QueryParameters

countApplication :: (MonadIO m, Val v) => Pipe -> v -> m Int
countApplication pipe application = access pipe master "amash" $ count $ select ["application" =: application] "rankings"

createApplication :: Pipe -> [Char] -> IO ()
createApplication pipe application = do
    access pipe master "amash" $ insert "rankings" ["application" =: application]
    putStrLn $ "[DB:RANKINGS] Created new application '" ++ application ++ "'."

createApplicationIfNotExist :: Pipe -> Application -> IO ()
createApplicationIfNotExist pipe application = do
    appCount <- countApplication pipe appString
    if appCount == 0
    then createApplication pipe appString
    else putStrLn $ "[DB:RANKINGS] Application '" ++ appString ++ "' already exists."
    where appString = show application

runSetup pipe = do
    putStrLn "Starting AMASH Database setup..."

    createApplicationIfNotExist pipe Confluence
    createApplicationIfNotExist pipe Jira
    createApplicationIfNotExist pipe Bitbucket

    -- TODO: initialize vendors and plugin keys ?

    putStrLn "AMASH Database setup finished."