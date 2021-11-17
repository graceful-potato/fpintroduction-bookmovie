{-
  Этот модуль содержит функции для обработки API запросов.
  В MVC паттерне их можно рассматривать как контроллеры.
-}
module Handlers where

import Control.Monad.IO.Class (MonadIO)

import App
import DB.Timetable
import DB.Seat
import DB.Preliminary
import DB.Booking

getTimetable :: MonadIO m => AppT m [TimeSlot]
getTimetable = getAllMoviesTimetable

getSeats :: MonadIO m => TimeSlotId -> AppT m [Seat]
getSeats = getSeatsByTimeSlotId

--                           Аргументы, захваченные из url'а
--                              vvvvvvvvvvvvvv    vvvvvv
postPreliminary :: MonadIO m => TimeSlotId -> SeatId -> AppT m BookingId
--                                                                 ^^^^^^^^^
--                                                       Тип возвращаемого значения
postPreliminary msId seatId = do
  booking <- createPreliminary msId seatId
  pure $ bookingId booking
