module API
  ( BookMovieAPI
  , bookMovieAPI
  , module API.Timetable
  , module API.Preliminary
  , module API.Checkout
  , module API.Refund
  , module API.Stats
  , module API.BatchPreliminary
  -- ^ экспортируем модули, чтобы их содержимое было доступно вместе с модулем `API`
  ) where

import Servant
import API.Timetable
import API.Preliminary
import API.Checkout
import API.Refund
import API.Stats
import API.BatchPreliminary

{-
  API тип всего приложения, который описывает API методы.
  API методы, как и серверные хэндлеры, комбинируются с помощью типа `:<|>`.

    > : i (:<|>)
    data (:<|>) a b = a :<|> b
-}
type BookMovieAPI
  = TimetableApi
  :<|> PreliminaryAPI
  :<|> CheckoutAPI
  :<|> RefundAPI
  :<|> StatsAPI
  :<|> BatchPreliminaryAPI

{-
  Proxy — это особенный тип, который не содержит данных.
  Он определен следующим образом в Data`.Proxy`:

    `data Proxy t = Proxy`

  Это позволяет создавать любой тип `Proxy t` с помощью конструктора `Proxy`.
  Это безопасная замена `undefined :: t`, потому что нет ошибок в рантайме.

  Сам тип бывает полезен, чтобы передать информацию о типах, необходимую только
  во время компиляции (данных в рантайме никаких нет).

  Servant использует очень много type-level конструкций и использование Proxy-типов — один из них.
  Это позволяет нам сообщать servant, что мы строим функции именно для нашего API: `BookMovieAPI`.
-}
bookMovieAPI :: Proxy BookMovieAPI
bookMovieAPI = Proxy
