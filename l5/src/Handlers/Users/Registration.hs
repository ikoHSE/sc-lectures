module Handlers.Users.Registration
  ( registerUser,
    logIn,
  )
where

import Control.Concurrent.STM.TVar
import Control.Monad.Reader
import Data.Aeson
import qualified Data.Map as M
import Data.Text (Text)
import GHC.Conc
import GHC.Generics
import Types
import Web.Cookie

registerUser ::
  (MonadReader ServerEnv m, MonadIO m, MonadRandom m) =>
  JsonBody UserRegistration ->
  m (ServerResponse NoBody)
registerUser (JsonBody u) = do
  let (k, v) = toUser u
  usersTVar <- asks users
  liftIO . atomically $ modifyTVar usersTVar $ M.insert k v
  logInUser k

data LogInErrors = UserNotFound | WrongPassword
  deriving (Show)

instance ServerError LogInErrors

logIn ::
  ( MonadReader ServerEnv m,
    MonadIO m,
    MonadRandom m,
    MonadError ResponseError m
  ) =>
  JsonBody UserLogIn ->
  m (ServerResponse NoBody)
logIn (JsonBody u) = do
  usersTVar <- asks users
  usersM <- liftIO . readTVarIO $ usersTVar
  let foundUser = M.lookup (logInUsername u) usersM
  case foundUser of
    Nothing -> throwServerError UserNotFound
    Just us ->
      if passwordHash us == hash (logInPassword u)
        then logInUser (logInUsername u)
        else throwServerError WrongPassword

logInUser ::
  ( MonadReader ServerEnv m,
    MonadIO m,
    MonadRandom m
  ) =>
  Username ->
  m (ServerResponse NoBody)
logInUser uName = do
  cookie <- generateCookie
  cookiesTVar <- asks cookies
  liftIO . atomically $ modifyTVar cookiesTVar $ M.insert (hash cookie) uName
  let setCookies =
        defaultSetCookie
          { setCookieName = authCookieNameBS,
            setCookieValue = encodeCookie cookie,
            setCookieMaxAge = Just 100000000,
            setCookiePath = Just "/"
          }
  return . addCookies setCookies $
    defaultServerResponse
      { responseBody = Just NoBody,
        responseStatus = status200
      }

data UserRegistration
  = UserRegistration
      { registrationUsername :: Username,
        registrationName :: Text,
        registrationPassword :: Password
      }
  deriving (Generic)

instance FromJSON UserRegistration

data UserLogIn
  = UserLogIn
      { logInUsername :: Username,
        logInPassword :: Password
      }
  deriving (Generic)

instance FromJSON UserLogIn

toUser :: UserRegistration -> (Username, User)
toUser u =
  ( registrationUsername u,
    User
      { name = registrationName u,
        passwordHash = hash $ registrationPassword u
      }
  )
