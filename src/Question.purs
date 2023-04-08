module Question where

import Api (encode)
import Data.Array (modifyAt, (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.Common (joinWith)
import Effect (Effect)
import Lib.Array (from)
import Lib.Crypto (crypto, randomUUID, getRandomValues)
import Lib.History (pushState)
import Lib.NetworkInformation (connection, downlink, hasConnection)
import Lib.Ninjas (randomImage)
import Lib.Peer (broadcast)
import Lib.React (cn, onChange)
import Prelude (Unit, bind, const, discard, pure, show, unit, ($), (&&), (/=), (<#>), (<$>), (<=), (<>), (=<<), (>), (>>=))
import Proto.Uint8Array (newUint8Array)
import React (ReactElement, getProps, getState, modifyState)
import React.DOM (button, div, input, span, text)
import React.DOM.Props (_type, autoFocus, onClick, placeholder, style, value)
import React.DOM.Props (Props) as R
import Types (Api(..), Nav(..), QuestionCardWithID, This)
import Web.HTML (window)
import Web.HTML.HTMLDocument (setTitle)
import Web.HTML.Window (document, navigator)

questionForm :: This -> Effect ReactElement
questionForm this = do
  state' <- getState this
  pure $
    div [ cn "form" ]
    [ input
      [ _type "text", placeholder $ state'.t "question", autoFocus true
      , value state'.question
      , onChange \v -> modifyState this _ { question = v }
      ]
    , button
      [ _type "button"
      , onClick \_ -> do
          state <- getState this
          let question = state.question
          if question /= "" then do
            props <- getProps this
            questionID <- randomUUID =<< crypto =<< window
            let questionCard = { title: question, background: Nothing }
            let questionCardWithID = { questionID, questionCard }
            let encoded = encode $ Question questionCardWithID
            broadcast props.peer encoded
            props.store.add "questions" encoded
            modifyState this \s -> s { questions = questionCardWithID : s.questions, question = "" }
            fetchImage this 0
          else pure unit
      ] [ text $ state'.t "post" ]
    ]

questionCards :: This -> Effect ReactElement
questionCards this = do
  state <- getState this
  pure $ div [ cn "questions" ] $ state.questions <#> questionCard

  where

  questionCard :: QuestionCardWithID -> ReactElement
  questionCard { questionID, questionCard: { title, background } } =
    div (
    [ cn "question question-link"
    , onClick $ const goToQuestion
    ] <> showImage) showTitle

    where

    goToQuestion :: Effect Unit
    goToQuestion = do
      pushState questionID $ "/post/"<>questionID
      window >>= document >>= setTitle title
      modifyState this _ { nav = ViewCard questionID }

    showTitle :: Array ReactElement
    showTitle = [ span [ cn "question-title" ] [ text title ] ]

    showImage :: Array R.Props
    showImage = case background of
      Nothing -> []
      (Just x) -> [ style { background: x } ]

fetchImage :: This -> Int -> Effect Unit
fetchImage this i = do
  navigator <- navigator =<< window
  speed <- hasConnection navigator >>= case _ of
    true -> downlink =<< connection navigator
    false -> pure 100.0
  if i <= 2 && speed > 5.0 then do
    randomImage i \url -> setImage $ "url(" <> url <> ")"
  else do
    let xs = newUint8Array 3
    getRandomValues xs =<< crypto =<< window
    setImage $ "rgba(" <> joinWith ", " (show <$> from xs) <> ", 0.3)"

  where

  setImage :: String -> Effect Unit
  setImage background = modifyState this \s -> s {
    questions = fromMaybe s.questions (modifyAt i (\x -> x {
      questionCard = x.questionCard { background = Just background }
    }) s.questions)
  }