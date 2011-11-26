-- | The "Logger" module provides a simple wrapper around
-- @hslogger@'s @System.Log.Logger@.
module Logger (
        -- * Logger configuration
        setupLogger,

        -- * Helpers
        debugM, noticeM, warningM, errorM
    ) where

import Control.Monad.IO.Class ( MonadIO(..) )
import System.Log.Logger ( Priority(..), setLevel, updateGlobalLogger )
import qualified System.Log.Logger as L

setupLogger :: IO ()
setupLogger = do
  updateGlobalLogger "FixId" (setLevel DEBUG)
  noticeM "Logger started"

debugM, noticeM, warningM, errorM :: (MonadIO m) => String -> m ()
debugM   = liftIO . L.debugM loggerName
noticeM  = liftIO . L.noticeM loggerName
warningM = liftIO . L.warningM loggerName
errorM   = liftIO . L.errorM loggerName

loggerName :: String
loggerName = "FixId"
