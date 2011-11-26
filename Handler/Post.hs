{-# LANGUAGE OverloadedStrings #-}

-- | The 'Post' handler serves the content of a post, or a form for
-- creating/updating a post.
module Handler.Post (
        post
    ) where

import Blaze.ByteString.Builder.Char.Utf8 ( fromText )
import Logger ( noticeM )
import Network.HTTP.Types ( statusOK )
import Network.Wai ( Application, Request(..), Response(..) )
import Text.Interpol ( (^-^) )

post :: Application
post req = do
  noticeM $ "Handling post " ^-^ (rawPathInfo req)
  return (ResponseBuilder statusOK [] (fromText "Viewing a post"))
