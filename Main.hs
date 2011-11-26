{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database ( setupDatabase )
import Handler.Index ( index )
import Handler.NotFound ( notFound )
import Handler.Post ( post )
import Handler.Resource ( resource )
import Logger ( setupLogger, noticeM )
import Network.Wai ( Application, Request(..) )
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
  -- FIXME Close the database here or something

router :: Conf -> Application
router conf req =
    let path = pathInfo req
    in case path of
         []               -> index conf req
         ("post" : path') -> post (req { pathInfo = path' })
         ("r" : path')    -> resource (req { pathInfo = path' })
         _                -> notFound req
