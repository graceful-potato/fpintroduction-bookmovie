module API.Checkout where

import Servant.API
import DB.Booking
import DB.DTO.Checkout

type CheckoutAPI = "api" :> "checkout"
                :> Capture "id" BookingId
                :> Post '[JSON] Checkout
                :<|>
                "api":> "checkout"
                :> ReqBody '[JSON] [BookingId]
                :> Post '[JSON] [Checkout]
