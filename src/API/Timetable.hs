module API.Timetable where

import Servant.API
import DB.Timetable
import DB.Seat

type TimetableApi
  = "api" :> "timetable" :> Get '[JSON] [TimeSlot]
  -- ^ метод для получения доступных сеансов
  :<|>
  -- ^ (:<|>) специальный тип для описания API, предоставляемый Servant'ом
  -- Он соединяет методы API
    ("api" :> "timetable" :> Capture "id" TimeSlotId :> "seats" :> Get '[JSON] [Seat])
  -- ^ метод для получения мест для конкретного сеанса с указанным id