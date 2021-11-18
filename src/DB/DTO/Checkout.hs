{-# LANGUAGE DeriveAnyClass #-}

module DB.DTO.Checkout where
  
import Data.Aeson
import Database.SQLite.Simple
import GHC.Generics
import DB.DTO.Booking
import Data.Text
import Data.Time

data Checkout = Checkout
  { bookingId :: BookingId
  , title :: Text
  , startTime :: UTCTime
  , row :: Integer
  , seat :: Integer
  } deriving (Eq, Show, Generic)

deriving instance FromRow Checkout
deriving instance ToRow Checkout

instance ToJSON Checkout
instance FromJSON Checkout
