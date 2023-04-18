module Types where

import Data.Map (Map)
import Data.Maybe (Maybe)
import Effect (Effect)
import Lib.Peer (Peer)
import Prelude (Unit)
import Proto.Uint8Array (Uint8Array)
import React (ReactThis)

type Props =
  { peer :: Peer
  , store :: Store
  , t :: String -> String
  }

type State =
  { questions :: Array QuestionCardWithID
  , question :: String
  , answer :: String
  , answers :: Answers
  , nav :: Nav
  }

type This = ReactThis Props State

data Nav = EmptyView | ViewCards | ViewCard QuestionID

type Store =
  { add :: Feed -> Uint8Array -> Effect Unit
  , all :: Feed -> (Array Api -> Effect Unit) -> Effect Unit
  }

type Feed = String

type QuestionID = String

type QuestionCard =
  { title :: String
  , background :: Maybe String
  }

type QuestionCardWithID =
  { questionID :: QuestionID
  , questionCard :: QuestionCard
  }

type Answer = String

type AnswerWithID =
  { questionID :: QuestionID
  , answer :: Answer
  }

type Answers = Map QuestionID (Array String)

data Api = Question QuestionCardWithID | Answer AnswerWithID | Ask QuestionID
