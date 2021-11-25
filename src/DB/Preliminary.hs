{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}

module DB.Preliminary where

import Database.SQLite.Simple

import DB.Booking
import DB.Timetable
import DB.Internal
import DB.Seat
import Data.Time.Clock
import Servant (err403, err404, err422)
import Utils (JSONError (JSONError), throwJSONError)
import API.BatchPreliminary
import qualified Data.Text as T


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
  seat <- seatWithBooking conn msId seatId
  processSeatWithBooking conn msId seat

createBatchPreliminary :: DBMonad m => BookingPayload -> m [Booking]
createBatchPreliminary (BookingPayload timeSlotId seatIds) = runSQL $ \conn -> do
  timeSlot <- query conn "SELECT * FROM timetable WHERE id = ?" timeSlotId :: IO [TimeSlot]
  checkTimeSlot timeSlot
  seatsWithBooking <- mapM (seatWithBooking conn timeSlotId) seatIds
  mapM (processSeatWithBooking conn timeSlotId) seatsWithBooking

checkTimeSlot :: [TimeSlot] -> IO ()
checkTimeSlot [] = throwJSONError err422 (JSONError "Timeslot is not found")
checkTimeSlot _ = pure ()

checkSeat :: [Seat] -> SeatId -> IO ()
checkSeat [] seatId = throwJSONError err422 $ JSONError ("Seat (" <> T.pack (show (unSeatId seatId)) <> ") is not found")
checkSeat _ _ = pure ()

checkBooking :: UTCTime -> [Booking] -> IO (Maybe Booking)
checkBooking _ [] = pure Nothing
checkBooking currTime (booking:_)
  | not $ isPreliminary booking = throwJSONError err403 $ JSONError ("Seat (" <> showSeatId booking <> ") already taken")
  | isReservationActive currTime booking = throwJSONError err403 $ JSONError ("Seat (" <> showSeatId booking <> ") already reserved")
  | otherwise = pure $ Just booking

addBooking :: Connection -> TimeSlotId -> SeatId -> IO Booking
addBooking conn msId seatId = do
  execute conn "INSERT INTO bookings (seat_id, time_slot_id, is_preliminary) values (?, ?, true)" (seatId, msId)
  result <- query conn "SELECT id, seat_id, time_slot_id, is_preliminary, created_at FROM bookings WHERE time_slot_id = ? AND seat_id = ?" (msId, seatId)
  case result of
    [] -> throwJSONError err404 (JSONError "Booking is not found")
    (x:_) -> pure x

showSeatId :: Booking -> T.Text
showSeatId booking = T.pack $ show $ unSeatId $ DB.Booking.seatId booking

seatWithBooking :: Connection -> TimeSlotId -> SeatId -> IO (SeatId, Maybe Booking)
seatWithBooking conn msId seatId = do
  seat <- query conn "SELECT * FROM seats WHERE id = ? AND available = true" seatId :: IO [Seat]
  checkSeat seat seatId
  booking <- query conn "SELECT * FROM bookings WHERE time_slot_id = ? AND seat_id = ?" (msId, seatId) :: IO [Booking]
  currTime <- getCurrentTime
  expBooking <- checkBooking currTime booking
  pure (seatId, expBooking)

processSeatWithBooking :: Connection -> TimeSlotId -> (SeatId, Maybe Booking) -> IO Booking
processSeatWithBooking conn timeSlotId (seatId, expiredBooking) = do
  case expiredBooking of
    Nothing -> addBooking conn timeSlotId seatId
    Just expBooking -> do
      deleteBooking conn expBooking
      addBooking conn timeSlotId seatId
