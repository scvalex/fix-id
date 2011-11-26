{-# LANGUAGE OverloadedStrings #-}

-- FIXME: Add some useful information to the page:
--   - what page was accessed;
--   - why the request failed (if reason is known);
--   - link to the front page (probably in a header);
--   - links to similarly name pages.

-- | The 'notFound' handler serves a kick-ass \"404 Not Found\" page.
module Handler.NotFound (
        notFound
    ) where

import Data.Text ( Text, pack )
import Logger ( noticeM )
import Network.HTTP.Types ( statusNotFound )
import Network.Wai ( Application, Request(..), Response(..) )
import Text.Blaze ( Html, ToHtml(..) )
import Text.Blaze.Html5 ( (!) )
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Renderer.Utf8 ( renderHtmlBuilder )
import Text.Interpol ( (^-^) )

notFound :: Application
notFound req = do
  noticeM $ "Not found " ^-^ (rawPathInfo req)
  return (ResponseBuilder statusNotFound [] . renderHtmlBuilder .
          notFoundPage . pack $ show req)

notFoundPage :: (ToHtml a) => a -> Html
notFoundPage comment = do
  H.docType
  H.html $ do
    H.head $ do
             H.title "404 Not Found"
             H.link ! A.rel "stylesheet" ! A.type_ "text/css"
                    ! A.href "/r/base.css"
             H.link ! A.rel "stylesheet" ! A.type_ "text/css"
                    ! A.href "/r/404.css"
    H.body $ do
             H.header $ do
                   H.div $ mapM_ number [398..401]
                   H.div $ mapM_ number [402..405]
                   H.div $ mapM_ number [406..409]
             H.article ! A.class_ "comment" $ do
                   H.div (toHtml comment)
     where
       number :: Int -> Html
       number 404 = H.span (toHtml ("404 " :: Text)) ! A.class_ "missing"
       number n   = H.span (toHtml n) >> toHtml (" " :: Text)
