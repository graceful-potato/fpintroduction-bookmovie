module DB.Booking
  ( module DB.Booking,
    module DB.DTO.Booking,
  )
where

import DB.DTO.Booking
import DB.DTO.Checkout
import API.Refund
import DB.Internal
import Data.Time.Clock
import Database.SQLite.Simple
import Servant (err403, err404)
import Utils
import qualified Data.Text as T

{-
  Booking запрос должен проверить наличие предварительного бронирования.
  Если оно существует и прошло меньше 10 минут от создания, то бронирование
  проходит успешно, иначе необходимо вернуть сообщение об ошибке в JSON формате.
-}
makeCheckout :: DBMonad m => BookingId -> m Checkout
makeCheckout bookingId = runSQL $ \conn -> do
  validateBooking conn bookingId
  createCheckout conn bookingId

makeBatchCheckout :: DBMonad m => [BookingId] -> m [Checkout]
makeBatchCheckout bookingIds = runSQL $ \conn -> do
  mapM_ (validateBooking conn) bookingIds
  mapM (createCheckout conn) bookingIds

validateBooking :: Connection -> BookingId -> IO ()
validateBooking conn bookingId = do
  booking <- query conn "SELECT * FROM bookings WHERE id = ?" bookingId :: IO [Booking]
  currTime <- getCurrentTime
  canCheckout currTime booking bookingId

createCheckout :: Connection -> BookingId -> IO Checkout
createCheckout conn bookingId = do
  execute conn "UPDATE bookings SET is_preliminary = false WHERE id = ?" bookingId
  checkout <- query conn checkoutQuery bookingId :: IO [Checkout]
  case checkout of
    [] -> throwJSONError err403 (JSONError "Something went wrong")
    (result:_) -> pure result

makeRefund :: DBMonad m => BookingId -> m Refund
makeRefund bookingId = runSQL $ \conn -> do
  booking <- fetchBooking conn bookingId
  deleteBooking conn booking
  pure $ Refund $ "Booking (" <> T.pack (show (unBookingId bookingId)) <> ") has been canceled"

makeBatchRefund :: DBMonad m => [BookingId] -> m [Refund]
makeBatchRefund bookingIds = runSQL $ \conn -> do
  bookings <- mapM (fetchBooking conn) bookingIds
  mapM_ (deleteBooking conn) bookings
  pure $ fmap (\booking -> Refund $ "Booking (" <> showBookingId booking <> ") has been canceled") bookings

fetchBooking :: Connection -> BookingId -> IO Booking
fetchBooking conn bookingId = do
  booking <- query conn "SELECT * FROM bookings WHERE id = ?" bookingId :: IO [Booking]
  case booking of
    [] -> throwJSONError err404 $ JSONError ("Booking (" <> T.pack (show (unBookingId bookingId)) <> ") not found")
    (result:_) -> pure result

deleteBooking :: Connection -> Booking -> IO ()
deleteBooking conn booking = do
  execute conn "DELETE FROM bookings WHERE id = ?" (DB.DTO.Booking.bookingId booking)

canCheckout :: UTCTime -> [Booking] -> BookingId -> IO ()
canCheckout _ [] bookingId = throwJSONError err404 $ JSONError ("Booking (" <> T.pack (show (unBookingId bookingId)) <> ") not found")
canCheckout currTime (booking : _) _
  | not $ isPreliminary booking = throwJSONError err403 $ JSONError ("Booking (" <> showBookingId booking <> ") already paid")
  | not $ isReservationActive currTime booking = throwJSONError err403 $ JSONError ("Booking (" <> showBookingId booking <> ") time expired")
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

showBookingId :: Booking -> T.Text
showBookingId booking = T.pack $ show $ unBookingId $ DB.DTO.Booking.bookingId booking
