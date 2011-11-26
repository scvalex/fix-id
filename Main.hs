{-# LANGUAGE OverloadedStrings #-}

module Main where

import Blaze.ByteString.Builder.Char.Utf8 ( fromText )
import Data.Monoid ( Monoid(..) )
import Data.Text ( pack )
import Network.HTTP.Types ( statusOK, statusNotFound )
import Network.Wai ( Application, Request(..), Response(..) )
import Network.Wai.Handler.Warp ( run )

main :: IO ()
main = run 50080 router

router :: Application
router req = let path = pathInfo req
             in case path of
                  []               -> index req
                  ("post" : path') -> viewPost (req { pathInfo = path' })
                  _                -> notFound req

index :: Application
index _ = return (ResponseBuilder statusOK [] (fromText "Main page"))

viewPost :: Application
viewPost _ = return (ResponseBuilder statusOK [] (fromText "Viewing a post"))

notFound :: Application
notFound req = return (ResponseBuilder statusNotFound []
                                       (fromText ("Not found" `mappend`
                                                  pack (show req))))
