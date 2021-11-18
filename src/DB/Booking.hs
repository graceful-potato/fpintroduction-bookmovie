module DB.Booking
  ( module DB.Booking,
    module DB.DTO.Booking,
  )
where

import DB.DTO.Booking
import DB.DTO.Checkout
import DB.DTO.Refund
import DB.Internal
import Data.Time.Clock
import Database.SQLite.Simple
import Servant (err403, err404)
import Utils

{-
  Booking запрос должен проверить наличие предварительного бронирования.
  Если оно существует и прошло меньше 10 минут от создания, то бронирование
  проходит успешно, иначе необходимо вернуть сообщение об ошибке в JSON формате.
-}
makeCheckout :: DBMonad m => BookingId -> m Checkout
makeCheckout bookingId = runSQL $ \conn -> do
  booking <- query conn "SELECT * FROM bookings WHERE id = ?" bookingId :: IO [Booking]
  currTime <- getCurrentTime
  canCheckout currTime booking
  execute conn "UPDATE bookings SET is_preliminary = false WHERE id = ?" bookingId
  checkout <- query conn checkoutQuery bookingId :: IO [Checkout]
  case checkout of
    [] -> throwJSONError err403 (JSONError "Something went wrong")
    (result:_) -> pure result

makeRefund :: DBMonad m => BookingId -> m Refund
makeRefund bookingId = runSQL $ \conn -> do
  booking <- query conn "SELECT * FROM bookings WHERE id = ?" bookingId :: IO [Booking]
  case booking of
    [] -> throwJSONError err404 (JSONError "Booking not found")
    (result:_) -> do
      deleteBooking conn result
      pure $ Refund "Booking has been canceled"

deleteBooking :: Connection -> Booking -> IO ()
deleteBooking conn booking = do
  execute conn "DELETE FROM bookings WHERE id = ?" (DB.DTO.Booking.bookingId booking)

canCheckout :: UTCTime -> [Booking] -> IO ()
canCheckout _ [] = throwJSONError err404 (JSONError "Booking not found")
canCheckout currTime (booking : _)
  | not $ isPreliminary booking = throwJSONError err403 (JSONError "Booking already paid")
  | not $ isReservationActive currTime booking = throwJSONError err403 (JSONError "Booking time expired")
  | otherwise = pure ()

reservationTime :: NominalDiffTime
reservationTime = 600

isReservationActive :: UTCTime -> Booking -> Bool
isReservationActive currTime booking = diffUTCTime currTime bookingTime < reservationTime
  where
    bookingTime = createdAt booking

checkoutQuery :: Query
checkoutQuery = 
  "SELECT bookings.id, "
    <> "timetable.title, "
    <> "timetable.start_time, "
    <> "seats.row, "
    <> "seats.seat "
    <> "FROM bookings "
    <> "INNER JOIN timetable ON bookings.time_slot_id = timetable.id "
    <> "INNER JOIN seats on bookings.seat_id = seats.id "
    <> "WHERE bookings.id = ?"
