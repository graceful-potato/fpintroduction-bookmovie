module API.Refund where

import Servant.API
import DB.DTO.Booking
import Data.Text
import Data.Aeson
import GHC.Generics (Generic)

type RefundAPI = "api" :> "refund"
              :> Capture "id" BookingId
              :> Get '[JSON] Refund
              :<|>
              "api" :> "refund"
              :> ReqBody '[JSON] [BookingId]
              :> Get '[JSON] [Refund]

data Refund = Refund { status :: Text } deriving (Eq, Show, Generic)

instance ToJSON Refund
instance FromJSON Refund
