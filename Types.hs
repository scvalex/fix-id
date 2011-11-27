{-# LANGUAGE OverloadedStrings #-}

module Types (
        Conf(..),

        stripPrefixReq
    ) where

import Database ( Database )
import Data.String ( IsString(..) )
import Data.Text ( stripPrefix, splitOn )
import qualified Data.Text as T
import Data.Text.Encoding ( decodeUtf8, encodeUtf8 )
import Network.Wai ( Request(..) )

data Conf = Conf { getDatabase :: Database }

stripPrefixReq :: String -> Request -> Maybe Request
stripPrefixReq pre req = do
    path <- stripPrefix (fromString pre) (decodeUtf8 $ rawPathInfo req)
    let path' = if T.null path then "/" else path
    return $ req { rawPathInfo = encodeUtf8 path'
                 , pathInfo    = splitOn "/" path' }
