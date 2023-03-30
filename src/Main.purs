module Main where

import Prelude hiding (div)
import Affjax.RequestHeader (RequestHeader(..))
import Data.Array (take, drop, modifyAt, (:))
import Data.FunctorWithIndex (mapWithIndex)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.MediaType (MediaType(..))
import Data.String.Common (joinWith, split, toLower)
import Data.String.Pattern (Pattern(Pattern))
import Data.Traversable (sequence)
import Data.Tuple (Tuple(Tuple))
import Effect (Effect)
import Effect.Exception (throw)
import Lib.Ajax (getEff, getBlobEff)
import Lib.React (cn, onChangeValue)
import Lib.Peer (Peer, initPeer, onConnection, onOpen, onData, broadcast)
import React (ReactClass, ReactElement, ReactThis, component, createLeafElement, getProps, getState, modifyState)
import React.DOM (button, div, input, text, span)
import React.DOM.Props (_type, autoFocus, onClick, placeholder, style, value)
import ReactDOM (render)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)
import Web.File.Url (createObjectURL)

type Props =
  { imagePath :: String
  , imageHeaders :: Array RequestHeader
  , peer :: Peer
  }

type State =
  { lang :: String
  , keyText :: String -> String
  , cards :: Array Card
  , question :: String
  }

type Card =
  { title :: String
  , image :: Maybe String
  }

type This = ReactThis Props State

appClass :: ReactClass Props
appClass = component "App" \this -> do
  pure
    { state: 
      { lang: "uk"
      , keyText: \key -> key
      , cards: [ { title: "Good morning!", image: Nothing }, { title: "Hello!", image: Nothing }, { title: "How are you?", image: Nothing }, { title: "???", image: Nothing } ]
      , question: ""
      } :: State
    , render: render this
    , componentDidMount: do
        setLang this "uk"
        props <- getProps this
        onConnection props.peer \conn ->
          onOpen conn $ onData conn \x ->
            modifyState this \s -> s { cards = { title: x, image: Nothing } : s.cards }
        void $ fetchImages this
    }
  where
  setLang :: This -> String -> Effect Unit
  setLang this lang = do
    getEff ("langs/" <> lang <> ".js") \v -> do
      let keys = Map.fromFoldable $ split (Pattern "\n") v <#> split (Pattern "=") <#> \kv -> Tuple (joinWith "" $ take 1 kv) (joinWith "" $ drop 1 kv)
      modifyState this _ { lang = lang, keyText = \key -> fromMaybe key $ Map.lookup (toLower key) keys }

  fetchImages :: This -> Effect (Array Unit)
  fetchImages this = do
    state <- getState this
    sequence $ mapWithIndex (\i _ -> fetchImage this i) state.cards

  fetchImage :: This -> Int -> Effect Unit
  fetchImage this index = do
    props <- getProps this
    getBlobEff props.imagePath props.imageHeaders \v -> do
      url <- createObjectURL v
      modifyState this \s -> s { cards = fromMaybe s.cards (modifyAt index (_ { image = Just url }) s.cards) }

  render :: This -> Effect ReactElement
  render this = do
    form <- showForm this
    cards <- showCards this
    pure $ 
      div [ ]
      [ form
      , cards
      ]

  showForm :: This -> Effect ReactElement
  showForm this = do
    state <- getState this
    props <- getProps this
    pure $
      div [ cn "form" ]
      [ input
        [ _type "text", placeholder $ state.keyText "question", autoFocus true
        , value state.question
        , onChangeValue \v -> modifyState this _ { question = v }
        ]
      , button
        [ _type "button"
        , onClick \_ -> do
            broadcast props.peer state.question
            modifyState this \s -> s { cards = { title: state.question, image: Nothing } : s.cards, question = "" }
            fetchImage this 0
        ] [ text $ state.keyText "post" ]
      ]

  showCards :: This -> Effect ReactElement
  showCards this = do
    state <- getState this
    let rows = map showCard state.cards
    pure $ div [ cn "cards" ] rows

  showCard :: Card -> ReactElement
  showCard { title, image: Nothing } =
    div [ cn "card" ] [ showTitle title ]
  showCard { title, image: Just url } =
    div [ cn "card", style { backgroundImage: "url("<>url<>")" } ] [ showTitle title ]

  showTitle :: String -> ReactElement
  showTitle title = span [ cn "card-title" ] [ text title ]

main :: Effect Unit
main = do
  doc <- window >>= document
  elem <- getElementById "container" $ toNonElementParentNode doc
  container <- maybe (throw "container not found") pure elem
  peer <- initPeer { host: "uaapps.xyz", port: 443, secure: true, path: "/board" }
  let props = {
      imagePath: "https://api.api-ninjas.com/v1/randomimage?category=nature&width=500&height=375"
    , imageHeaders:
      [ Accept $ MediaType "image/jpg"
      , RequestHeader "X-Api-Key" "0YJRpHZcyfY185HL1U2cPA==kJYRAKy5BmMcEWHD"
      ]
    , peer: peer
    }
  let element = createLeafElement appClass props
  void $ render element container
