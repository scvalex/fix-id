{-# LANGUAGE OverloadedStrings #-}

-- | The 'resource' handler just serves static files from the @.\/r\/@
-- directory.  It expects the 'pathInfo' part of the 'Request' to be
-- the path to the file under @.\/r\/@.  I.e. any other path prefix
-- should have been stripped already.
module Handler.Resource (
        resource
    ) where

import Data.Text ( stripPrefix, splitOn )
import Data.Text.Encoding ( decodeUtf8, encodeUtf8 )
import Logger ( noticeM )
import Network.Wai ( Application, Request(..) )
import Network.Wai.Application.Static ( StaticSettings(..)
                                      , staticApp, defaultFileServerSettings
                                      , fileSystemLookup )
import Text.Interpol ( (^-^) )

resource :: Application
resource req = do
  let (Just path) = stripPrefix "/r/" (decodeUtf8 $ rawPathInfo req)
  noticeM $ "Serving resource " ^-^ path
  staticApp defaultFileServerSettings
               { ssFolder  = fileSystemLookup "r"
               , ssListing = Nothing
               , ssIndices = [] } req { rawPathInfo = encodeUtf8 path
                                      , pathInfo    = splitOn "/" path }
