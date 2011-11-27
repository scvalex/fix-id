{-# LANGUAGE OverloadedStrings #-}

-- | The 'index' handler serves the main page.
module Handler.Index (
        index
    ) where

import Database ( Post, getRecentPostsM, getPostM )
import Data.Maybe ( fromJust )
import Data.Text ( Text )
import Handler.Response ( basicPage, mkResponse )
import Logger ( noticeM )
import Network.Wai ( Application )
import Text.Blaze ( Html, ToHtml(..) )
import Text.Blaze.Html5 ( (!) )
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Types ( Conf(..) )

index :: Conf -> Application
index Conf{getDatabase = db} _ = do
  noticeM "Serving index page"
  postNums <- getRecentPostsM db 10
  posts <- return . (map fromJust) =<< mapM (getPostM db) postNums
  return $ mkResponse (indexPage posts)

indexPage :: [Post] -> Html
indexPage posts = do
  basicPage "fix . id :: a" (return ()) $ do
    H.header $ do
      H.h1 "fix . id :: a"
    H.article ! A.class_ "comment" $ do
      H.h2 (toHtml ("Posts" :: Text))
      H.ul $ mapM_ (H.li . toHtml) posts
