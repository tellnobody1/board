module Question where

import Api (Api(Question), CardWithID, encode)
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
import Types
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
            cardID <- randomUUID =<< crypto =<< window
            let card = { title: question, background: Nothing }
            let cardWithID = { cardID, card }
            let encoded = encode $ Question cardWithID
            broadcast props.peer encoded
            props.store.add "questions" encoded
            modifyState this \s -> s { cards = cardWithID : s.cards, question = "" }
            fetchImage this 0
          else pure unit
      ] [ text $ state'.t "post" ]
    ]

questionCards :: This -> Effect ReactElement
questionCards this = do
  state <- getState this
  pure $ div [ cn "cards" ] $ state.cards <#> questionCard

  where

  questionCard :: CardWithID -> ReactElement
  questionCard { cardID, card } =
    div (
    [ cn "card card-link"
    , onClick $ const goToQuestion
    ] <> showImage card.background) showTitle

    where

    goToQuestion :: Effect Unit
    goToQuestion = do
      pushState cardID $ "/post/"<>cardID
      window >>= document >>= setTitle card.title
      modifyState this _ { nav = ViewCard cardID }

    showTitle :: Array ReactElement
    showTitle = [ span [ cn "card-title" ] [ text card.title ] ]   

    showImage :: Maybe String -> Array R.Props
    showImage Nothing = []
    showImage (Just background) = [ style { background: background } ]

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
  setImage background = modifyState this \s -> s { cards = fromMaybe s.cards (modifyAt i (\x -> x { card = x.card { background = Just background } }) s.cards) }