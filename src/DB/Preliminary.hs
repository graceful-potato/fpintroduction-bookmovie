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
  timeSlot <- query conn "SELECT * FROM timetable where id = ?" msId :: IO [TimeSlot]
  checkTimeSlot timeSlot
  seat <- query conn "SELECT * FROM seats where id = ? and available = true" seatId :: IO [Seat]
  checkSeat seat
  booking <- query conn "SELECT * FROM bookings where time_slot_id = ? and seat_id = ?" (msId, seatId) :: IO [Booking]
  time <- fmap (addUTCTime (-600)) getCurrentTime
  expiredBooking <- checkBooking time booking
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
checkBooking time (booking:_)
  | not $ isPreliminary booking = throwJSONError err403 (JSONError "Seat already taken")
  | createdAt booking > time = throwJSONError err403 (JSONError "Seat already reserved")
  | otherwise = pure $ Just booking

addBooking :: Connection -> TimeSlotId -> SeatId -> IO Booking
addBooking conn msId seatId = do
  execute conn "INSERT INTO bookings (seat_id, time_slot_id, is_preliminary) values (?, ?, true)" (seatId, msId)
  result <- query conn "SELECT id, seat_id, time_slot_id, is_preliminary, created_at FROM bookings where time_slot_id = ? and seat_id = ?" (msId, seatId)
  case result of
    [] -> throwJSONError err404 (JSONError "Booking is not found")
    (x:_) -> pure x

deleteBooking :: Connection -> Booking -> IO ()
deleteBooking conn booking = do
  execute conn "DELETE FROM bookings where id = ?" (bookingId booking)
