{-# LANGUAGE OverloadedStrings #-}

-- | The 'Post' handler serves the content of a post, or a form for
-- creating/updating a post.
module Handler.Post (
        post, postFragment
    ) where

import Data.Text ( Text )
import Handler.NotFound ( notFound )
import Logger ( noticeM )
import Network.HTTP.Types ( statusOK )
import Network.Wai ( Application, Request(..), Response(..) )
import Network.Wai.Middleware.Route ( dispatch, (&~~) )
import Text.Blaze ( Html, ToHtml(..) )
import Text.Blaze.Html5 ( (!) )
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Renderer.Utf8 ( renderHtmlBuilder )
import Text.Interpol ( (^-^) )
import Types ( stripPrefixReq )

post :: Application
post req = do
  let (Just req') = stripPrefixReq "/post" req
  noticeM $ "Handling post " ^-^ (rawPathInfo req')
  dispatch [ ("GET" &~~ "^/$", newPost)
           , ("POST" &~~ "^/$", newPostPOST)
           , ("GET" &~~ "^/[0-9]+", showPost)
           , ("POST" &~~ "^/[0-9]+", updatePost)
           ] notFound req'

newPost :: Application
newPost _ = return (ResponseBuilder statusOK [] $ renderHtmlBuilder postPage)

newPostPOST :: Application
newPostPOST = undefined

showPost :: Application
showPost = undefined

updatePost :: Application
updatePost = undefined

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
