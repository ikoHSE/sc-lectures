module Server (runServer) where

import Control.Concurrent.STM.TVar
import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Data.List
import qualified Data.Map as M
import Data.Text (Text)
import Handlers
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.Handler.Warp
import Orphans ()
import Types
import Web.Cookie

runServer :: IO ()
runServer = do
  usersTVar <- newTVarIO M.empty
  cookiesTVar <- newTVarIO M.empty
  postsTVar <- newTVarIO []
  let serverEnv =
        ServerEnv
          { users = usersTVar,
            cookies = cookiesTVar,
            posts = postsTVar
          }
  run 8000 $ \req respF -> do
    putStr $ BSC.unpack (rawPathInfo req) <> "\t"
    resp <- flip runReaderT serverEnv $ router (requestMethod req) (pathInfo req) req
    respF resp

runHandler ::
  (FromResponseBody a, ToResponseBody b, MonadIO m) =>
  (a -> ExceptT ResponseError m (ServerResponse b)) ->
  Request ->
  m Response
runHandler f req = do
  rBody <- liftIO $ lazyRequestBody req
  case fromBody rBody of
    Right a -> do
      liftIO $ putStrLn "OK"
      either fromServerResponse fromServerResponse <$> runExceptT (f a)
    Left e -> do
      liftIO $ putStrLn "bad body"
      return $ responseLBS status422 [] (BSLC.pack e)

runAuthHandler ::
  (FromResponseBody a, ToResponseBody b, MonadIO m, MonadReader ServerEnv m) =>
  (Username -> a -> ExceptT ResponseError m (ServerResponse b)) ->
  Request ->
  m Response
runAuthHandler f req = do
  let authCookie =
        fmap snd
          . find ((==) authCookieNameBS . fst)
          . mconcat
          . fmap (parseCookies . snd)
          . filter ((==) "Cookie" . fst)
          $ requestHeaders req
      decodedCookie = authCookie >>= either (const Nothing) Just . decodeCookie
  cookiesTVar <- asks cookies
  cs <- liftIO $ readTVarIO cookiesTVar
  let
  case hash <$> decodedCookie >>= flip M.lookup cs of
    Nothing -> return $ responseLBS unauthorized401 [] "Unauthorized"
    Just u -> runHandler (f u) req

fromServerResponse :: ToResponseBody b => ServerResponse b -> Response
fromServerResponse (ServerResponse b s h) = responseLBS s h (maybe "" toBody b)

type Path = [Text]

router :: Method -> Path -> Request -> ServerM Response
router "POST" ["users", "register"] = runHandler registerUser
router "POST" ["users", "log_in"] = runHandler logIn
router "GET" ["posts"] = runAuthHandler viewPosts
router "POST" ["posts"] = runAuthHandler newPost
router _ _ = \_ -> do
  liftIO $ putStrLn "not found"
  return $ responseLBS status404 [] "Not found"

type ServerM a = ReaderT ServerEnv IO a
