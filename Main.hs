{-# LANGUAGE OverloadedStrings #-}

module Main where

import Blaze.ByteString.Builder.Char.Utf8 ( fromText )
import Database ( setupDatabase )
import Handler.Index ( index )
import Handler.NotFound ( notFound )
import Handler.Resource ( resource )
import Logger ( setupLogger, noticeM )
import Network.HTTP.Types ( statusOK )
import Network.Wai ( Application, Request(..), Response(..) )
import Network.Wai.Handler.Warp ( run )
import Types ( Conf(..) )

-- FIXME Add some sort of supervision to the server process

main :: IO ()
main = do
  setupLogger
  db <- setupDatabase
  noticeM "Starting FixId on 50080"
  let conf = Conf { getDatabase = db }
  run 50080 (router conf)

router :: Conf -> Application
router conf req =
    let path = pathInfo req
    in case path of
         []               -> index conf req
         ("post" : path') -> viewPost (req { pathInfo = path' })
         ("r" : path')    -> resource (req { pathInfo = path' })
         _                -> notFound req

viewPost :: Application
viewPost _ = return (ResponseBuilder statusOK [] (fromText "Viewing a post"))
