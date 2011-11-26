{-# LANGUAGE OverloadedStrings #-}

-- | The 'resource' handler just serves static files from the @.\/r\/@
-- directory.  It expects the 'pathInfo' part of the 'Request' to be
-- the path to the file under @.\/r\/@.  I.e. any other path prefix
-- should have been stripped already.
module Handler.Resource (
        resource
    ) where

import Network.Wai ( Application, Request(..) )
import Network.Wai.Application.Static ( StaticSettings(..)
                                      , staticApp, defaultFileServerSettings
                                      , fileSystemLookup )

resource :: Application
resource req = let path = pathInfo req
               in staticApp defaultFileServerSettings
                      { ssFolder  = fileSystemLookup "r"
                      , ssListing = Nothing
                      , ssIndices = [] } req
