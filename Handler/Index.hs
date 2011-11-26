{-# LANGUAGE OverloadedStrings #-}

-- | The 'index' handler serves the main page.
module Handler.Index (
        index
    ) where

import Data.Text ( Text )
import Logger ( noticeM )
import Network.HTTP.Types ( statusOK )
import Network.Wai ( Application, Response(..) )
import Text.Blaze ( Html, ToHtml(..) )
import Text.Blaze.Html5 ( (!) )
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Renderer.Utf8 ( renderHtmlBuilder )

index :: Application
index _ = do
  noticeM "Serving index page"
  return (ResponseBuilder statusOK [] $ renderHtmlBuilder indexPage)

indexPage :: Html
indexPage = do
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
               H.div (toHtml ("Lorem" :: Text))
