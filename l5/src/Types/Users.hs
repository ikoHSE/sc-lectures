module Types.Users
  ( module Types.Hash,
    module Crypto.Random,
    Username (..),
    User (..),
    Password (..),
    RandomCookie (..),
    generateCookie,
    encodeCookie,
    decodeCookie,
    authCookieNameT,
    authCookieNameBS,
  )
where

import Crypto.Random
import Data.Aeson
import Data.ByteString
import qualified Data.ByteString.Base64 as B64
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import GHC.Generics (Generic)
import Types.Hash

newtype Username
  = Username {unUsername :: Text}
  deriving (FromJSON, ToJSON, Ord, Eq)

newtype Password
  = Password {unPassword :: Text}
  deriving (FromJSON, ToJSON, Ord, Eq, Hashable)

data User
  = User
      { name :: Text,
        passwordHash :: Hash Password
      }
  deriving (Generic)

newtype RandomCookie
  = RandomCookie {unRandomCookie :: ByteString}
  deriving (Hashable, Eq, Ord)

cookieSize :: Int
cookieSize = 512

generateCookie :: MonadRandom m => m RandomCookie
generateCookie = RandomCookie <$> getRandomBytes cookieSize

encodeCookie :: RandomCookie -> ByteString
encodeCookie = B64.encode . unRandomCookie

decodeCookie :: ByteString -> Either String RandomCookie
decodeCookie = fmap RandomCookie . B64.decode

authCookieNameBS :: ByteString
authCookieNameBS = T.encodeUtf8 authCookieNameT

authCookieNameT :: Text
authCookieNameT = "auth"
