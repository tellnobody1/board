module Answer where

import Api (encode)
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
import Types (Answer, Answers, Api(..), QuestionCardWithID, QuestionID, This)

answersPage :: This -> QuestionCardWithID -> Effect ReactElement
answersPage this { questionID, questionCard: { title } } = do
  state' <- getState this
  pure $
    div []
    [ h1 [ cn "question-header" ] [ text title ]
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
              let encoded = encode $ Answer { questionID, answer }
              broadcast props.peer encoded
              props.store.add "answers" encoded
              modifyState this _ { answers = addAnswers state.answers questionID answer, answer = "" }
            else pure unit
        ] [ text $ state'.t "post" ]
      ]
    , ol [ cn "answers" ] $ showAnswer <$> answers state'.answers
    ]

  where

  answers :: Answers -> Array String
  answers = Map.lookup questionID >>> fromMaybe []

  showAnswer :: String -> ReactElement
  showAnswer = li [] <<< singleton <<< text

addAnswers :: Answers -> QuestionID -> Answer -> Answers
addAnswers answers questionID answer = Map.alter (case _ of
  Just xs -> Just $ answer : xs
  Nothing -> Just $ singleton answer) questionID answers
