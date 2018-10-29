{-# LANGUAGE OverloadedStrings #-}

-- | Wraps the wreq API in some simple functions since I do not yet fully understand the Lens API.
module WreqUtil (
        responseIsOkay,
        responseGetStatus,
        responseGetBody
    ) where

import Network.Wreq
import Control.Lens
import Data.ByteString.Internal

-- | Checks whether or not a wreq response is okay (status code 200).
responseIsOkay
    :: Response body -- ^ The wreq response.
    -> Bool          -- ^ Whether or not the response is okay.
responseIsOkay r = r ^. responseStatus . statusCode == 200

-- | Extracts both the status code and the status message from a wreq response.
responseGetStatus
    :: Response body     -- ^ The wreq response.
    -> (Int, ByteString) -- ^ StatusCode and StatusMessage
responseGetStatus r = (r ^. responseStatus . statusCode, r ^. responseStatus . statusMessage)

-- | Extracts the body from a wreq response.
responseGetBody
    :: Response body -- ^ The wreq response.
    -> body          -- ^ The body of the Response
responseGetBody r = r ^. responseBody
