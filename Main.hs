module Main where

import Data.Monoid ( Monoid(..) )
import Blaze.ByteString.Builder.Char.Utf8 ( fromText )
import Network.HTTP.Types ( statusOK )
import Network.Wai ( Application, Request(..), Response(..) )
import Network.Wai.Handler.Warp ( run )

main :: IO ()
main = run 50080 fixId

fixId :: Application
fixId req = let path = pathInfo req
            in return (ResponseBuilder statusOK [] (fromText $ mconcat path))
