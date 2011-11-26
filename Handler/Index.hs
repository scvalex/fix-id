{-# LANGUAGE OverloadedStrings #-}

-- | The 'index' handler serves the main page.
module Handler.Index (
        index
    ) where

import Database ( Post, getRecentPostsM, getPostM )
import Data.Maybe ( fromJust )
import Data.Text ( Text )
import Logger ( noticeM )
import Network.HTTP.Types ( statusOK )
import Network.Wai ( Application, Response(..) )
import Text.Blaze ( Html, ToHtml(..) )
import Text.Blaze.Html5 ( (!) )
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Renderer.Utf8 ( renderHtmlBuilder )
import Types ( Conf(..) )

index :: Conf -> Application
index Conf{getDatabase = db} _ = do
  noticeM "Serving index page"
  postNums <- getRecentPostsM db 10
  posts <- return . (map fromJust) =<< mapM (getPostM db) postNums
  return (ResponseBuilder statusOK [] $ renderHtmlBuilder (indexPage posts))

indexPage :: [Post] -> Html
indexPage posts = do
  H.docType
  H.html $ do
    H.head $ do
             H.title "fix . id :: a"
             H.link ! A.rel "stylesheet" ! A.type_ "text/css"
                    ! A.href "/r/base.css"
    H.body $ do
             H.header $ do
               H.h1 "fix . id :: a"
             H.article ! A.class_ "comment" $ do
               H.h2 (toHtml ("Posts" :: Text))
               H.ul $ mapM_ (H.li . toHtml) posts
