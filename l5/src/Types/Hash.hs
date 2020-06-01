module Types.Hash
  ( hash,
    Hash,
    Hashable (..),
  )
where

import qualified Crypto.Hash as H
import Crypto.Hash.Algorithms
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import Data.Text (Text)
import qualified Data.Text.Encoding as T

newtype Hash r = Hash (H.Digest SHA3_256)
  deriving (Eq, Ord)

class Hashable x where
  toBS :: x -> ByteString

hash :: (Hashable x) => x -> Hash x
hash = Hash . H.hash . toBS

instance Hashable ByteString where
  toBS = id

instance Hashable BSL.ByteString where
  toBS = BSL.toStrict

instance Hashable Text where
  toBS = toBS . T.encodeUtf8
