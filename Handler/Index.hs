{-# LANGUAGE OverloadedStrings #-}

-- | The 'index' handler serves the main page.
module Handler.Index (
        index
    ) where

import Blaze.ByteString.Builder.Char.Utf8 ( fromText )
import Logger ( noticeM )
import Network.HTTP.Types ( statusOK )
import Network.Wai ( Application, Response(..) )

index :: Application
index _ = do
  noticeM "Serving index page"
  return (ResponseBuilder statusOK [] (fromText "Main page"))
