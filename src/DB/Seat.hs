{-# LANGUAGE DeriveAnyClass #-}

module DB.Seat
  ( module DB.Seat
  , module DB.DTO.Seat)
  where

import Database.SQLite.Simple

import DB.DTO.Seat
import DB.Timetable
import DB.Internal

getSeatsByTimeSlotId
  :: DBMonad m
  => TimeSlotId     -- Для выполнения запроса используется функция из DB.Internal
  -> m [Seat]           -- vvvvvv                     Для передачи в запрос параметра используется `?`
getSeatsByTimeSlotId msId = runSQL $ \conn ->       --                                               v  vvvv
  query conn "SELECT id, row, seat, available, time_slot_id from seats where time_slot_id = ?" msId
-- ^^^
-- Функция, выполняющая SQL запрос https://hackage.haskell.org/package/sqlite-simple-0.4.18.0/docs/Database-SQLite-Simple.html#v:query
