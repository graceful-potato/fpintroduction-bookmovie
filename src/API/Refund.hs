module API.Refund where

import Servant.API
import DB.Booking
import DB.DTO.Refund

type RefundAPI = "api" 
              :> "refund"
              :> Capture "id" BookingId
              :> Get '[JSON] Refund
