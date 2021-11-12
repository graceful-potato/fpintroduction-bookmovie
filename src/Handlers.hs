{-
  Этот модуль содержит функции для обработки API запросов.
  В MVC паттерне их можно рассматривать как контроллеры.
-}
module Handlers where

import Control.Monad.IO.Class (MonadIO)
import Servant.Server

import App
import DB.Timetable
import DB.Seat
import DB.Preliminary
import DB.Booking
import Utils

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
  bookings <- createPreliminary msId seatId
  case bookings of
    (b:_) -> pure $ bookingId b
    --   Возвращаем ошибку в формете JSON
    --   vvvvvvvvvvvvvv          vvvvvvvvv
    _ -> throwJSONError err404 $ JSONError "booking is not found"
    --                  ^^^^^^
    -- Http-код ошибки 404, err404 :: ServantErr
