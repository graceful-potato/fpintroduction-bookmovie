module App where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Concurrent.STM

-- Тип обертка для пути к базе данных
newtype DatabasePath = DatabasePath { unDatabasePath :: String } deriving Show

-- Конфигурация приложения
data Config = Config
  { dbPath :: DatabasePath
  , reqCounter :: TVar Int
  }

{-
 Трансформер приложения для комбинации монад, нужных для работы
-}
newtype AppT m a = AppT
  { runApp :: ReaderT Config m a
  } deriving
    ( Functor, Applicative, Monad
    , MonadReader Config
    -- ^^^^^^^^^^
    -- выводим instance, чтобы получить метод ask, для удобного доступа к конфигурации
    , MonadIO
    -- ^^^^^^
    -- Содержит единственный метод liftIO :: MonadIO m => IO a -> m a,
    -- с помощью которого можно запускать IO методы, не задумываясь
    -- о количестве трансформеров в стэке
    )
