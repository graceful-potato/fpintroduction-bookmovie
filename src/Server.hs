module Server where

import Control.Monad.Catch as C (Handler(..), SomeException(..), catches, displayException)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Except (ExceptT(..))
import Control.Monad.Reader (runReaderT)
import Servant as S

import App (AppT(..), Config)
import API (BookMovieAPI, bookMovieAPI)
import Handlers (getTimetable, getSeats, postPreliminary, postCheckout, deleteBooking, getStats)
import Utils (toServerError)
{-
  Для сервера мы используем библиотеку servant-server. Которая предоставляет нам
  трансформер `ServerT`, где первым параметром идет тип, описывающий наше API. В нашем случае
  это `BookMovieAPI`, определенный в `API.hs`. Вторым параметром идет уже наш трансформер
  для приложения `AppT` из модуля `App`.

  Сам сервер — это набор хэндлеров, соединенных с помощью оператора `:<|>`.
-}
bookingServer :: MonadIO m => ServerT BookMovieAPI (AppT m)
bookingServer = (getTimetable
  :<|> getSeats)
  :<|> postPreliminary
  :<|> postCheckout
  :<|> deleteBooking
  :<|> getStats
{-
  Функция, которая создает servant приложение `Application`.
-}
mkApplication :: Config -> Application
-- Значение, с помощью которого мы сообщаем Servant'у тип BookMovieAPI (см. API.hs)
--                           vvvvvvvvvvvv
mkApplication config = serve bookMovieAPI mkServer
  --                   ^^^^^
  where -- Функции, предоставляемые Servant'ом
    --         vvvvvvvvvvv
    mkServer = hoistServer bookMovieAPI (convertApp config) bookingServer

{-
  Эта функция конвертирует наше вычисление, завернутое в трансформер `AppT`,
  в серверный хэндлер библиотеки servant `S.Handler`.

  Конвертируем мы его, последовательно разворачивая наши трансформеры
  - `runApp app` для AppT
  - `runReaderT` для ReaderT, передавая конфигурацию приложения
  - `catchErrors` для `ExceptT`, навешивая обработку исключений,
    чтобы выводить сообщения об ошибках в stdout.

  Мы можем расширить `errHandler` до более серьезного логгирования например при помощи библиотеки
  `katip`, которая позволит нам складывать логи в Elasticsearch для удобного поиска.
-}
convertApp :: Config -> AppT IO a -> S.Handler a
convertApp config app = catchErrors $ runReaderT (runApp app) config
  where
    errHandler :: C.Handler IO (Either ServerError a)
    errHandler = C.Handler $ \(SomeException e) -> do
      print e
      pure $ Left $ toServerError $ displayException e

    jsonErrHandler :: C.Handler IO (Either ServerError a)
    jsonErrHandler = C.Handler $ \e@ServerError { errBody = errBody } -> do
      print $ "Error: " <> errBody
      pure $ Left e

    catchErrors :: IO a -> S.Handler a
    catchErrors act = S.Handler $ ExceptT $
      catches (act >>= return . Right) [jsonErrHandler, errHandler]
