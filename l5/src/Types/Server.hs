module Types.Server
  ( module Network.HTTP.Types.Status,
    module Network.HTTP.Types.Header,
    ServerResponse (..),
    defaultServerResponse,
    addCookies,
    ServerEnv (..),
    ToResponseBody (..),
    FromResponseBody (..),
    NoBody (..),
    JsonBody (..),
    ServerError (..),
    throwServerError,
    ResponseError,
    MonadError,
  )
where

import Control.Concurrent.STM.TVar
import Control.Monad.Except
import qualified Data.Aeson as A
import Data.Bifunctor
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Network.HTTP.Types.Header
import Network.HTTP.Types.Status
import Types.Post
import Types.Users
import Web.Cookie

data ServerEnv
  = ServerEnv
      { users :: TVar (Map Username User),
        cookies :: TVar (Map (Hash RandomCookie) Username),
        posts :: TVar [Post]
      }

data ServerResponse a
  = ServerResponse
      { responseBody :: Maybe a,
        responseStatus :: Status,
        responseHeaders :: ResponseHeaders
      }

defaultServerResponse :: ServerResponse a
defaultServerResponse =
  ServerResponse
    { responseBody = Nothing,
      responseStatus = status200,
      responseHeaders = []
    }

addCookies :: SetCookie -> ServerResponse a -> ServerResponse a
addCookies c r =
  r
    { responseHeaders = ("Set-Cookie", newCookies) : responseHeaders r
    }
  where
    newCookies = BSL.toStrict . BS.toLazyByteString $ renderSetCookie c

class ToResponseBody r where
  toBody :: r -> ByteString

class FromResponseBody r where
  fromBody :: ByteString -> Either String r

data NoBody = NoBody

instance ToResponseBody NoBody where
  toBody _ = ""

instance FromResponseBody NoBody where
  fromBody _ = return NoBody

data JsonBody a = JsonBody {unJsonBody :: a}

instance A.ToJSON a => ToResponseBody (JsonBody a) where
  toBody (JsonBody a) = A.encode a

instance A.FromJSON a => FromResponseBody (JsonBody a) where
  fromBody = fmap JsonBody . A.eitherDecode

instance ToResponseBody ByteString where
  toBody = id

instance FromResponseBody ByteString where
  fromBody = return

instance ToResponseBody Text where
  toBody = BSL.fromStrict . T.encodeUtf8

instance FromResponseBody Text where
  fromBody = first show . T.decodeUtf8' . BSL.toStrict

class ServerError e where
  serverErrorStatus :: e -> Status
  serverErrorStatus _ = status422

throwServerError ::
  (MonadError (ServerResponse ByteString) m, ServerError e, Show e) =>
  e ->
  m a
throwServerError e =
  throwError $
    defaultServerResponse
      { responseBody = Just . BSLC.pack . show $ e,
        responseStatus = serverErrorStatus e
      }

type ResponseError = ServerResponse ByteString
