{-# LANGUAGE OverloadedStrings #-}

module Main where

import Blaze.ByteString.Builder.Char.Utf8 ( fromText )
import Handler.NotFound ( notFound )
import Handler.Resource ( resource )
import Logger ( setupLogger, noticeM )
import Network.HTTP.Types ( statusOK )
import Network.Wai ( Application, Request(..), Response(..) )
import Network.Wai.Handler.Warp ( run )

main :: IO ()
main = do
  setupLogger
  noticeM "Starting FixId on 50080"
  run 50080 router

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
