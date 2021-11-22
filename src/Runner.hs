module Runner where

import Network.Wai.Handler.Warp
import Network.Wai.Logger (withStdoutLogger)
import System.IO

import App
import Server
import Control.Concurrent.STM

{-
  Данный модуль содержит главную функцию, которая запускает web-приложение.
  Она вызывается в `bin/Main.hs`. Здесь происходит настройка веб-сервера wai,
  которому мы передаем настройки, указав номер порта и простой логгер, который
  логгирует информацию в stdout.

  С помощью `appConfig` конфигурируется web-приложение, что позволяет нам
  расширять его новыми параметрами, задавая их с помощью CLI (считывать параметры командной строки).

  Само приложение создается с помощью `mkApplication` из модуля `Server`.
-}
run :: IO ()
-- Выполняет функцию, передавая ей на вход logger
--    vvvvvvvvvvvvvvv
run = withStdoutLogger $ \logger -> do
  counter <- newTVarIO 0
  let
    port = 3000
    waiSettings =
      -- Задаём порт
      setPort port $
      -- Пишем отладочную информацию на консоль
      setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
      -- Указываем logger
      setLogger logger defaultSettings
    -- Создаём конфиг
    appConfig = Config
      { dbPath = DatabasePath "bookmovie.db"
      , reqCounter = counter
      }
  -- запускаем приложение с помощью wai

  runSettings waiSettings (mkApplication appConfig)
  --                       ^^^^^^^^^^^^^
  -- Функция из модуля Server, которая создаёт значение,
  -- необходимое Servant'у для запуска API
