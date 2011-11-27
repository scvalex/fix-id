{-# LANGUAGE OverloadedStrings #-}

-- | The 'resource' handler just serves static files from the @.\/r\/@
-- directory.  It expects the 'pathInfo' part of the 'Request' to be
-- the path to the file under @.\/r\/@.  I.e. any other path prefix
-- should have been stripped already.
module Handler.Resource (
        resource
    ) where

import Logger ( noticeM )
import Network.Wai ( Application, Request(..) )
import Network.Wai.Application.Static ( StaticSettings(..)
                                      , staticApp, defaultFileServerSettings
                                      , fileSystemLookup )
import Text.Interpol ( (^-^) )
import Types ( stripPrefixReq )

resource :: Application
resource req = do
  let (Just req') = stripPrefixReq "/r/" req
  noticeM $ "Serving resource " ^-^ (rawPathInfo req')
  staticApp defaultFileServerSettings
               { ssFolder  = fileSystemLookup "r"
               , ssListing = Nothing
               , ssIndices = [] } req'
