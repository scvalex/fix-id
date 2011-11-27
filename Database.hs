{-# LANGUAGE TypeFamilies, DeriveDataTypeable, TemplateHaskell #-}

-- | The "Database" module provides a high-level interface for
-- querying and updating the database.  Internally, we use
-- @acid-state@ but that should not be the concern of any code outside
-- this module.
module Database (
        -- * Types
        Database, Post,

        -- * Configuration
        setupDatabase,

        -- * Queries
        getPostM, getRecentPostsM,

        -- * Updates
        addPostM
    ) where

import Control.Monad.Reader ( MonadReader(..) )
import Control.Monad.State ( MonadState(..) )
import Control.Monad.IO.Class ( MonadIO(..) )
import Data.Acid ( AcidState, Update, Query, makeAcidic
                 , openLocalStateFrom, update, query )
import Data.IntMap ( IntMap )
import qualified Data.IntMap as IM
import Data.List ( sort )
import Data.SafeCopy ( deriveSafeCopySimple, base )
import Logger ( noticeM )

type Database = AcidState MyState

type Post = String

data MyState = MyState { getPosts    :: IntMap Post
                       , getNextPost :: Int }

newState :: MyState
newState = MyState { getPosts    = IM.empty
                   , getNextPost = 1 }

$(deriveSafeCopySimple 1 'base ''MyState)

addPost :: Post -> Update MyState Int
addPost post = do
  state@MyState{ getPosts    = posts
               , getNextPost = postNum } <- get
  let postNum' = postNum + 1
  put $ state { getPosts    = IM.insert postNum post posts
              , getNextPost = postNum' }
  return postNum

getPost :: Int -> Query MyState (Maybe Post)
getPost num = do
  MyState{getPosts = posts} <- ask
  return $ IM.lookup num posts

getRecentPosts :: Int -> Query MyState [Int]
getRecentPosts limit = do
  MyState{getPosts = posts} <- ask
  return . take limit . reverse . sort $ IM.keys posts

$(makeAcidic ''MyState ['addPost, 'getPost, 'getRecentPosts])

addPostM :: (MonadIO m) => Database -> Post -> m Int
addPostM db post = liftIO $ update db (AddPost post)

getPostM :: (MonadIO m) => Database -> Int -> m (Maybe Post)
getPostM db num = liftIO $ query db (GetPost num)

getRecentPostsM :: (MonadIO m) => Database -> Int -> m [Int]
getRecentPostsM db limit = liftIO $ query db (GetRecentPosts limit)

setupDatabase :: IO Database
setupDatabase = do
  noticeM "Opening database"
  openLocalStateFrom databaseDir newState

databaseDir :: String
databaseDir = "db"
