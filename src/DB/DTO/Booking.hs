{-# LANGUAGE DeriveAnyClass #-}

module DB.DTO.Booking where

import Data.Aeson
import Data.Time
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.FromField
import Servant.API
import GHC.Generics

import DB.Timetable
import DB.Seat

{-
  Тип для идентификатора бронирования
-}
newtype BookingId = BookingId
  { unBookingId :: Integer }
  deriving (Eq, Show)
  -- Позволяет преобразовывать `BookingId` из строки БД
  --       vvvvv
  deriving ToRow via (Only Integer)
  --             ^^^
  -- Говорит о том, что `BookingId` нужно добавить в ToRow аналогично `Integer`.
  -- Позволяет получить значение типа `BookingId` из параметра в url
  --        vvvvvvvvvvvvvvv
  deriving (FromHttpApiData, FromField, ToField, FromJSON, ToJSON) via Integer
  --                         ^^^^^^^^^  ^^^^^^^
  -- Конвертация SQL-значения в значение Haskell
  -- https://hackage.haskell.org/package/sqlite-simple-0.4.18.0/docs/Database-SQLite-Simple-FromField.html

{-
  Record, описывающий таблицу `bookings`
-}
data Booking = Booking
  { bookingId :: BookingId
  , seatId :: SeatId
  , isPreliminary :: Bool
  , timeSlotId :: TimeSlotId
  , createdAt :: UTCTime
  } deriving (Eq, Show, Generic)
--                      ^^^^^^^
-- Класс Generic отвечает за универсальное кодирование типа, т.е. за  такое представление,
-- в котором используются конструкторы типов из ограниченного набора
-- https://hackage.haskell.org/package/base-4.14.0.0/docs/GHC-Generics.html
-- Это представление используется при выводе instance'ов других классов

-- Для записи и чтение данных типа `Booking` из базы
-- Как FromField и ToField, но для типов с несколькими полями
deriving instance FromRow Booking
deriving instance ToRow Booking

-- Эти instance'ы нужны для конвертации типа в JSON и обратно
-- Обратите внимание, что для добавления типа в эти классы не нужно
-- реализовывать ни одной функции, это значит, что у каждой функции
-- есть стандартная реализация.
instance ToJSON Booking
instance FromJSON Booking