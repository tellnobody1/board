module Answer where

import Api (Answer, Api(Answer), CardID, CardWithID, encode)
import Data.Array (singleton, (:))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Lib.Peer (broadcast)
import Lib.React (cn, onChange)
import Prelude (bind, discard, pure, unit, ($), (/=), (<$>), (<<<), (>>>))
import React (ReactElement, getProps, getState, modifyState)
import React.DOM (h1, button, div, input, text, ol, li)
import React.DOM.Props (_type, autoFocus, onClick, placeholder, value)
import Types

answersPage :: This -> CardWithID -> Effect ReactElement
answersPage this { cardID, card } = do
  state' <- getState this
  pure $
    div []
    [ h1 [ cn "card-header" ] [ text card.title ]
    , div [ cn "form" ]
      [ input
        [ _type "text", placeholder $ state'.t "answer", autoFocus true
        , value state'.answer
        , onChange \v -> modifyState this _ { answer = v }
        ]
      , button
        [ _type "button"
        , onClick \_ -> do
            state <- getState this
            let answer = state.answer
            if answer /= "" then do
              props <- getProps this
              let encoded = encode $ Answer { cardID, answer }
              broadcast props.peer encoded
              props.store.add "answers" encoded
              modifyState this _ { answers = addAnswers state.answers cardID answer, answer = "" }
            else pure unit
        ] [ text $ state'.t "post" ]
      ]
    , ol [ cn "answers" ] $ showAnswer <$> answers state'.answers
    ]

  where

  answers :: Answers -> Array String
  answers = Map.lookup cardID >>> fromMaybe []

  showAnswer :: String -> ReactElement
  showAnswer = li [] <<< singleton <<< text

addAnswers :: Answers -> CardID -> Answer -> Answers
addAnswers answers cardID answer = Map.alter (case _ of
  Just xs -> Just $ answer : xs
  Nothing -> Just $ singleton answer) cardID answers
