module DB.Internal where

import Control.Monad.Reader
import Database.SQLite.Simple

import App


-- Синоним-констрейнт, чтобы объединить по смыслу констрейнты,
-- необходимые для работы с базой данных.
--   vvvvvvv
type DBMonad m = (MonadIO m, MonadReader Config m)
--                ^^^^^^^
-- Класс, предоставляющий функцию liftIO, которая оборачивает IO в любую монаду
-- Её использование вы увидите ниже в функции runSQL
-- > import Control.Monad.IO.Class
-- > :i MonadIO

{-
  Функция высшего порядка, принимающяя действие для работы
  с базой данных `Connection -> IO a`, в теле которой мы
  достаем путь к базе данных, открываем соединение и передаем
  его в `action`, с помощью функции `withConnection`.

  Эта функция позволяет нам не доставать конфигурацию
  и не открывать соединение вручную каждый раз.
-}
runSQL
  :: DBMonad m
  => (Connection -> IO a)
  -> m a
runSQL action = do
  -- Это эквивалентно тому, что мы вытащили все поля
  -- Config'а и положили их в переменные с такими же именами
  -- (ниже dbPath из Config'а используется как переменная)
  --    vvvv
  Config{..} <- ask
  --            ^^^
  -- Используем монаду Reader для получения настроек
  -- Будет работать для любой монады m
  -- liftIO :: IO a -> m a
  -- v
  liftIO $ withConnection (unDatabasePath dbPath) action
