{-# LANGUAGE OverloadedStrings #-}

module AMASH.MongoDB.Setup (runSetup') where

import Database.MongoDB
import Control.Monad.IO.Class
import AMASH.Constants
import Data.Char

-- | Returns the count of documents in the rankings database for a given application.
countApplication :: (MonadIO m, Val v)
    => Pipe  -- ^ The pipe used to connect to the database.
    -> v     -- ^ The application.
    -> m Int -- ^ The count.
countApplication pipe application = access pipe master "amash" $ count $ select ["application" =: application] "rankings"

-- | Creates a new document for a given application name.
createApplication :: Pipe   -- ^ The pipe used to connect to the database.
                  -> [Char] -- ^ The application.
                  -> IO ()
createApplication pipe application = do
    access pipe master "amash" $ insert_ "rankings" ["application" =: application]
    putStrLn $ "[DB:rankings] Created new application '" ++ application ++ "'."

-- | Creates a new document for a given application if it doesn't already exist.
createApplicationIfNotExist :: Pipe        -- ^ The pipe used to connect to the database.
                            -> Application -- ^ The application.
                            -> IO ()
createApplicationIfNotExist pipe application = do
    appCount <- countApplication pipe appString

    if appCount == 0
    then createApplication pipe appString
    else putStrLn $ "[DB:rankings] Application '" ++ appString ++ "' already exists."
    where appString = map (toLower) (show application)

-- | Runs the database setup. The database setup currently just creates necessary empty documents in the rankings database.
runSetup' :: Pipe  -- ^ The pipe used to connect to the database.
          -> IO ()
runSetup' pipe = do
    putStrLn "Starting AMASH Database setup..."

    createApplicationIfNotExist pipe Confluence
    createApplicationIfNotExist pipe Jira
    createApplicationIfNotExist pipe Bitbucket

    putStrLn "AMASH Database setup finished."