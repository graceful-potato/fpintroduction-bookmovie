module API.Preliminary where

import Servant.API
import DB.Timetable
import DB.Seat
import DB.Booking

-- Метод для создания предварительного бронирования
-- GET api/timetable/{timeSlotId}/preliminary-booking/{seatId}
--                         ^^^^^^^^^^^^^^                       ^^^^^^
-- Эта части пути захватывается как аргументы, необходимые для обработки запроса
type PreliminaryAPI
  = "api" :> "timetable"
    :> Capture "id" TimeSlotId
    :> "preliminary-booking"
    :> Capture "id" SeatId
    :> Post '[JSON] BookingId
--  ^   ^     ^       ^
--  |   |     |   Тип возвращаемого значения
--  |   |  Cписок используемых content-type'ов https://hackage.haskell.org/package/servant-0.17/docs/Servant-API-ContentTypes.html
--  |   |  С помощью ' можно перечислить список, содержащий несколько типов
--  |  http-метод
-- (:>) специальный тип для описания методов API, предоставляемый Servant'ом
