module API.Checkout where

import Servant.API
import DB.Booking
import DB.DTO.Checkout

type CheckoutAPI = "api" :> "checkout"
                :> Capture "id" BookingId
                :> Get '[JSON] Checkout
                :<|>
                "api":> "checkout"
                :> ReqBody '[JSON] [BookingId]
                :> Get '[JSON] [Checkout]
