module Types where

import Api (Api, CardID, CardWithID)
import Data.Map (Map)
import Effect (Effect)
import Lib.Peer (Peer)
import Prelude (Unit)
import Proto.Uint8Array (Uint8Array)
import React (ReactThis)

type Props =
  { peer :: Peer
  , store :: Store
  }

type State =
  { lang :: String
  , t :: String -> String
  , cards :: Array CardWithID
  , question :: String
  , answer :: String
  , answers :: Answers
  , nav :: Nav
  }

type This = ReactThis Props State

data Nav = EmptyView | ViewCards | ViewCard CardID

type Store =
  { add :: Feed -> Uint8Array -> Effect Unit
  , all :: Feed -> (Array Api -> Effect Unit) -> Effect Unit
  }

type Feed = String

type Answers = Map CardID (Array String)
