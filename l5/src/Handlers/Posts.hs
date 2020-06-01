module Handlers.Posts
  ( viewPosts,
    newPost,
  )
where

import Control.Concurrent.STM.TVar
import Control.Monad.Reader
import Data.Text (Text)
import Data.Time.Clock
import GHC.Conc
import Types

viewPosts ::
  (MonadReader ServerEnv m, MonadIO m) =>
  Username ->
  NoBody ->
  m (ServerResponse (JsonBody [Post]))
viewPosts _ _ = do
  postsTVar <- asks posts
  postsList <- liftIO . readTVarIO $ postsTVar
  return defaultServerResponse {responseBody = Just (JsonBody postsList)}

newPost ::
  (MonadReader ServerEnv m, MonadIO m) =>
  Username ->
  Text ->
  m (ServerResponse NoBody)
newPost u p = do
  now <- liftIO getCurrentTime
  postsTVar <- asks posts
  liftIO . atomically $
    modifyTVar postsTVar (Post {author = u, body = p, timestamp = now} :)
  return defaultServerResponse
