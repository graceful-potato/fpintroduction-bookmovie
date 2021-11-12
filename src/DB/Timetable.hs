module DB.Timetable
  ( module DB.Timetable
  , module DB.DTO.TimeSlot)
  where

import DB.Internal
import DB.DTO.TimeSlot
import Database.SQLite.Simple

{-
  Метод для получения всех сеансов из базы данных.
-}
getAllMoviesTimetable
  :: DBMonad m
  => m [TimeSlot]
getAllMoviesTimetable = runSQL $ \conn ->
  query_ conn "SELECT id, title, start_time, duration from timetable"
