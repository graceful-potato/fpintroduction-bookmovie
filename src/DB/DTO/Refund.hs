module DB.DTO.Refund where

import Data.Text
import Data.Aeson
import GHC.Generics (Generic)

data Refund = Refund { status :: Text } deriving (Eq, Show, Generic)

instance ToJSON Refund
instance FromJSON Refund
