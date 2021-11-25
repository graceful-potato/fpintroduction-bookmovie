{-
  Этот модуль содержит функции для обработки API запросов.
  В MVC паттерне их можно рассматривать как контроллеры.
-}
module Handlers where


import App
import DB.Timetable
import DB.Seat
import DB.Preliminary
import DB.Booking
import DB.DTO.Checkout
import Control.Concurrent.STM
import Control.Monad.Reader
import API.Stats
import API.BatchPreliminary
import API.Refund

getTimetable :: MonadIO m => AppT m [TimeSlot]
getTimetable = do
  updateCounter
  getAllMoviesTimetable

getSeats :: MonadIO m => TimeSlotId -> AppT m [Seat]
getSeats slotId = do
  updateCounter
  getSeatsByTimeSlotId slotId

--                           Аргументы, захваченные из url'а
--                              vvvvvvvvvvvvvv    vvvvvv
postPreliminary :: MonadIO m => TimeSlotId -> SeatId -> AppT m BookingId
--                                                                 ^^^^^^^^^
--                                                       Тип возвращаемого значения
postPreliminary msId seatId = do
  updateCounter
  booking <- createPreliminary msId seatId
  pure $ DB.Booking.bookingId booking

postCheckout :: MonadIO m => BookingId -> AppT m Checkout
postCheckout bookingId = do
  updateCounter
  makeCheckout bookingId

postBatchCheckout :: MonadIO m => [BookingId] -> AppT m [Checkout]
postBatchCheckout bookingIds = do
  updateCounter
  makeBatchCheckout bookingIds

deleteBooking :: MonadIO m => BookingId -> AppT m Refund
deleteBooking bookingId = do
  updateCounter
  makeRefund bookingId

deleteBatchBooking :: MonadIO m => [BookingId] -> AppT m [Refund]
deleteBatchBooking bookingIds = do
  updateCounter
  makeBatchRefund bookingIds

getStats :: MonadIO m => AppT m Stats
getStats = do
  Config { reqCounter = counter } <- ask
  result <- liftIO $ readTVarIO counter
  pure $ Stats result

postBatchPreliminary :: MonadIO m => BookingPayload -> AppT m [BookingId]
postBatchPreliminary payload = do
  result <- createBatchPreliminary payload
  pure $ fmap DB.Booking.bookingId result

updateCounter :: MonadIO m => AppT m ()
updateCounter = do
  Config { reqCounter = counter } <- ask
  liftIO $ atomically $ modifyTVar counter (+1)
