{-# LANGUAGE OverloadedStrings #-}

module Handler.Resource (
        resource
    ) where

import Network.Wai ( Application )
import Network.Wai.Application.Static ( StaticSettings(..)
                                      , staticApp, defaultFileServerSettings
                                      , fileSystemLookup )

resource :: Application
resource = staticApp defaultFileServerSettings
             { ssFolder  = fileSystemLookup "r"
             , ssListing = Nothing
             , ssIndices = [] }
