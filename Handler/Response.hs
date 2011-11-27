{-# LANGUAGE OverloadedStrings #-}

-- | This module forms an ad-hoc templating system.
module Handler.Response (
        basicPage, mkResponse
    ) where

import Network.HTTP.Types ( statusOK )
import Network.Wai ( Response(..) )
import Text.Blaze ( Html, ToHtml(..) )
import Text.Blaze.Html5 ( (!) )
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Renderer.Utf8 ( renderHtmlBuilder )

mkResponse :: Html -> Response
mkResponse = ResponseBuilder statusOK [] . renderHtmlBuilder

basicPage :: String -> Html -> Html -> Html
basicPage title extraHead extraBody = do
  H.docType
  H.html $ do
    H.head $ do
             H.title (toHtml title)
             H.link ! A.rel "stylesheet" ! A.type_ "text/css"
                    ! A.href "/r/base.css"
             H.link ! A.rel "icon" ! A.type_ "image/png"
                    ! A.href "/r/logo.png"
             extraHead
    H.body $ extraBody

