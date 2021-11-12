module DB.Booking
  ( module DB.Booking
  , module DB.DTO.Booking)
  where

import DB.DTO.Booking
import DB.Internal

{-
  Booking запрос должен проверить наличие предварительного бронирования.
  Если оно существует и прошло меньше 10 минут от создания, то бронирование
  проходит успешно, иначе необходимо вернуть сообщение об ошибке в JSON формате.
-}
tryBook
  :: DBMonad m
  => BookingId
  -> m Bool
tryBook = undefined
