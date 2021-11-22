module API.Stats where

import Servant.API
import GHC.Generics
import Data.Aeson

data Stats = Stats { requestCounter :: Int } deriving (Eq, Show, Generic)

instance ToJSON Stats

type StatsAPI = "api" 
              :> "stats"
              :> Get '[JSON] Stats
