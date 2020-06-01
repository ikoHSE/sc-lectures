module Types.Post
  ( Post (..),
  )
where

import Data.Aeson
import Data.Text (Text)
import Data.Time.Clock
import GHC.Generics
import Types.Users

data Post
  = Post
      { author :: Username,
        body :: Text,
        timestamp :: UTCTime
      }
  deriving (Generic)

instance ToJSON Post
