{-# LANGUAGE OverloadedStrings #-}

module Main where

import Blaze.ByteString.Builder.Char.Utf8 ( fromText )
import Data.Text ( Text, pack )
import Text.Blaze ( Html, ToHtml(..) )
import Text.Blaze.Html5 ( (!) )
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Renderer.Utf8 ( renderHtmlBuilder )
import Network.HTTP.Types ( statusOK, statusNotFound )
import Network.Wai ( Application, Request(..), Response(..) )
import Network.Wai.Application.Static ( StaticSettings(..)
                                      , staticApp, defaultFileServerSettings
                                      , fileSystemLookup )
import Network.Wai.Handler.Warp ( run )

main :: IO ()
main = run 50080 router

router :: Application
router req = let path = pathInfo req
             in case path of
                  []               -> index req
                  ("post" : path') -> viewPost (req { pathInfo = path' })
                  ("r" : path')    -> resource (req { pathInfo = path' })
                  _                -> notFound req

index :: Application
index _ = return (ResponseBuilder statusOK [] (fromText "Main page"))

viewPost :: Application
viewPost _ = return (ResponseBuilder statusOK [] (fromText "Viewing a post"))

resource :: Application
resource = staticApp defaultFileServerSettings
             { ssFolder  = fileSystemLookup "r"
             , ssListing = Nothing
             , ssIndices = [] }

notFound :: Application
notFound req =
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
