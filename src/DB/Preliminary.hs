{-# LANGUAGE DeriveAnyClass #-}

module DB.Preliminary where

import Database.SQLite.Simple

import DB.Booking
import DB.Timetable
import DB.Internal
import DB.Seat
import Data.Time.Clock
import Servant (err403, err404, err422)
import Utils (JSONError (JSONError), throwJSONError)

{-
  Preliminary запрос должен создать предварительное бронирование, если
  заданное место на сеанс не занято. Если бронирование уже существует,
  необходимо вернуть сообщение об ошибке в JSON формате.
-}
createPreliminary
  :: DBMonad m
  => TimeSlotId
  -> SeatId
  -> m Booking
createPreliminary msId seatId = runSQL $ \conn -> do
  timeSlot <- query conn "SELECT * FROM timetable WHERE id = ?" msId :: IO [TimeSlot]
  checkTimeSlot timeSlot
  seat <- query conn "SELECT * FROM seats WHERE id = ? and available = true" seatId :: IO [Seat]
  checkSeat seat
  booking <- query conn "SELECT * FROM bookings WHERE time_slot_id = ? and seat_id = ?" (msId, seatId) :: IO [Booking]
  currTime <- getCurrentTime
  expiredBooking <- checkBooking currTime booking
  case expiredBooking of
    Nothing -> addBooking conn msId seatId
    Just expBooking -> do
      deleteBooking conn expBooking
      addBooking conn msId seatId

checkTimeSlot :: [TimeSlot] -> IO ()
checkTimeSlot [] = throwJSONError err422 (JSONError "Timeslot is not found")
checkTimeSlot _ = pure ()

checkSeat :: [Seat] -> IO ()
checkSeat [] = throwJSONError err422 (JSONError "Seat is not found")
checkSeat _ = pure ()

checkBooking :: UTCTime -> [Booking] -> IO (Maybe Booking)
checkBooking _ [] = pure Nothing
checkBooking currTime (booking:_)
  | not $ isPreliminary booking = throwJSONError err403 (JSONError "Seat already taken")
  | isReservationActive currTime booking = throwJSONError err403 (JSONError "Seat already reserved")
  | otherwise = pure $ Just booking

addBooking :: Connection -> TimeSlotId -> SeatId -> IO Booking
addBooking conn msId seatId = do
  execute conn "INSERT INTO bookings (seat_id, time_slot_id, is_preliminary) values (?, ?, true)" (seatId, msId)
  result <- query conn "SELECT id, seat_id, time_slot_id, is_preliminary, created_at FROM bookings WHERE time_slot_id = ? and seat_id = ?" (msId, seatId)
  case result of
    [] -> throwJSONError err404 (JSONError "Booking is not found")
    (x:_) -> pure x

deleteBooking :: Connection -> Booking -> IO ()
deleteBooking conn booking = do
  execute conn "DELETE FROM bookings WHERE id = ?" (bookingId booking)
