{-# LANGUAGE DeriveAnyClass #-}

module DB.DTO.Seat where

import Data.Aeson
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.FromField
import Servant.API
import GHC.Generics

import DB.Timetable

newtype SeatId = SeatId
  { unSeatId :: Integer }
  deriving (Eq, Show)
  deriving ToRow via (Only Integer)
  deriving (FromHttpApiData, FromField, ToField, FromJSON, ToJSON)
    via Integer

data Seat = Seat
  { seatId :: SeatId
  , row :: Integer
  , seat :: Integer
  , available :: Bool
  , timeSlotId :: TimeSlotId
  } deriving (Eq, Show, Generic)

deriving instance FromRow Seat
deriving instance ToRow Seat

instance ToJSON Seat
instance FromJSON Seat
