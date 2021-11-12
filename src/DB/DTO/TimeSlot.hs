{-# LANGUAGE DeriveAnyClass #-}

module DB.DTO.TimeSlot where

import Data.Aeson
import Data.Text
import Data.Time
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.FromField
import Servant.API
import GHC.Generics

newtype TimeSlotId = TimeSlotId
  { unTimeSlotId :: Integer }
  deriving (Eq, Show)
  deriving ToRow via (Only Integer)
  deriving (FromHttpApiData, FromField, ToField, FromJSON, ToJSON)
    via Integer

data TimeSlot = TimeSlot
  { timeSlotId :: TimeSlotId
  , title :: Text
  , start_time :: UTCTime
  , duration :: Integer
  } deriving (Eq, Show, Generic)

deriving instance FromRow TimeSlot
deriving instance ToRow TimeSlot

instance ToJSON TimeSlot
instance FromJSON TimeSlot