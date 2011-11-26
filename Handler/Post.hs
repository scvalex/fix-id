{-# LANGUAGE OverloadedStrings #-}

-- | The 'Post' handler serves the content of a post, or a form for
-- creating/updating a post.
module Handler.Post (
        post, postFragment
    ) where

import Data.Text ( Text )
import Logger ( noticeM )
import Network.HTTP.Types ( statusOK )
import Network.Wai ( Application, Request(..), Response(..) )
import Text.Blaze ( Html, ToHtml(..) )
import Text.Blaze.Html5 ( (!) )
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Renderer.Utf8 ( renderHtmlBuilder )
import Text.Interpol ( (^-^) )

post :: Application
post req = do
  noticeM $ "Handling post " ^-^ (rawPathInfo req)
  return (ResponseBuilder statusOK [] $ renderHtmlBuilder postPage)

postPage :: Html
postPage = do
  H.docType
  H.html $ do
    H.head $ do
             H.title "New Post"
    H.body $ do
             postFragment

postFragment :: Html
postFragment = do
  H.form ! A.action "/post" ! A.method "POST" $ do
    H.div $
     H.textarea ! A.rows "20" ! A.cols "80" $
      toHtml ("Insightful comment" :: Text)
    H.button (toHtml ("Submit" :: Text))
