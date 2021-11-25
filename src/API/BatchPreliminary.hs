module API.BatchPreliminary where

import Servant.API
import DB.Timetable
import DB.Seat
import DB.Booking
import GHC.Generics
import Data.Aeson

type BatchPreliminaryAPI
  = "api" :> "preliminary-booking"
    :> ReqBody '[JSON] BookingPayload
    :> Post '[JSON] [BookingId]

data BookingPayload = BookingPayload { timeSlotId :: TimeSlotId
                                     , seatIds :: [SeatId] } deriving (Eq, Show, Generic)

instance FromJSON BookingPayload
instance ToJSON BookingPayload
