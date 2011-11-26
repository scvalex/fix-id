{-# LANGUAGE OverloadedStrings #-}

-- | The 'resource' handler just serves static files from the @.\/r\/@
-- directory.  It expects the 'pathInfo' part of the 'Request' to be
-- the path to the file under @.\/r\/@.  I.e. any other path prefix
-- should have been stripped already.
module Handler.Resource (
        resource
    ) where

import Data.Monoid ( Monoid(..) )
import Logger ( noticeM )
import Network.Wai ( Application, Request(..) )
import Network.Wai.Application.Static ( StaticSettings(..)
                                      , staticApp, defaultFileServerSettings
                                      , fileSystemLookup )
import Text.Interpol ( (^-^) )

resource :: Application
resource req = do
  -- FIXME Make the following log command handle multi-item paths
  -- correctly
  noticeM $ "Serving resource " ^-^ mconcat (pathInfo req)
  staticApp defaultFileServerSettings
               { ssFolder  = fileSystemLookup "r"
               , ssListing = Nothing
               , ssIndices = [] } req
