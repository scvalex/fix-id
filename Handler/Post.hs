{-# LANGUAGE OverloadedStrings #-}

-- | The 'Post' handler serves the content of a post, or a form for
-- creating/updating a post.
module Handler.Post (
        post, postFragment
    ) where

import Control.Applicative ( (<$>) )
import Control.Monad.IO.Class ( liftIO )
import qualified Data.ByteString as BS
import Data.ByteString.Lazy.Char8 ( fromChunks, unpack )
import Data.Enumerator ( Iteratee )
import Data.Enumerator.Binary ( consume )
import qualified Data.URLEncoded as URL
import Data.Text ( Text )
import Database ( Database, Post, addPostM, getPostM )
import Handler.NotFound ( notFound, notFoundResponse )
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
import Types ( Conf(..), stripPrefixReq )

post :: Conf -> Application
post conf req = do
  let (Just req') = stripPrefixReq "/post" req
  noticeM $ "Handling post " ^-^ (rawPathInfo req')
  dispatch [ ("GET" &~~ "^/$", newPost)
           , ("POST" &~~ "^/$", newPostPOST conf)
           , ("GET" &~~ "^/[0-9]+$", showPost conf)
           , ("POST" &~~ "^/[0-9]+$", updatePost)
           ] notFound req'

newPost :: Application
newPost _ =
    return (ResponseBuilder statusOK [] $ renderHtmlBuilder newPostPage)

newPostPOST :: Conf -> Application
newPostPOST Conf{getDatabase = db} _ = do
  urlEncodedForm <- unpack <$> consume
  form <- liftIO $ URL.importString urlEncodedForm
  let (Just content) = URL.lookup ("content" :: String) form
  num <- addPostM db content
  noticeM $ "Creating post number " ^-^ (num :: Int)
  showPost' db num

showPost :: Conf -> Application
showPost Conf{getDatabase = db} req = do
  let num = read . tail . unpack . fromChunks $ [rawPathInfo req]
  showPost' db num

showPost' :: Database -> Int -> Iteratee BS.ByteString IO Response
showPost' db num = do
  mpost <- getPostM db num
  case mpost of
    Nothing ->
        return $ notFoundResponse ("No such post" :: String)
    Just p ->
        return $ ResponseBuilder statusOK [] . renderHtmlBuilder
               $ postPage p

updatePost :: Application
updatePost = undefined

newPostPage :: Html
newPostPage = do
  H.docType
  H.html $ do
    H.head $ do
             H.title "New Post"
    H.body $ do
             newPostFragment

newPostFragment :: Html
newPostFragment = do
  H.form ! A.action "/post" ! A.method "post" $ do
    H.div $
     H.textarea ! A.rows "20" ! A.cols "80" ! A.name "content" $
      toHtml ("Insightful comment" :: Text)
    H.button (toHtml ("Submit" :: Text)) ! A.value "Submit"

postPage :: Post -> Html
postPage p = do
  H.docType
  H.html $ do
    H.head $ do
             H.title "Post"
    H.body $ do
             postFragment p

postFragment :: Post -> Html
postFragment p = H.div ! A.class_ "post" $ toHtml p
