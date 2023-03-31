module Main where

import Affjax.RequestHeader (RequestHeader(..))
import Api (Api(Post), Card, decode, encode)
import Data.Array (take, drop, modifyAt, (:))
import Data.Either (Either(Right))
import Data.FunctorWithIndex (mapWithIndex)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, fromJust)
import Data.MediaType (MediaType(..))
import Data.String.Common (joinWith, split, toLower)
import Data.String.Pattern (Pattern(Pattern))
import Data.Traversable (sequence)
import Data.Tuple (Tuple(Tuple))
import Effect (Effect)
import Lib.Affjax (getEff, getBlobEff)
import Lib.Peer (Peer, newPeer, onConnection, onOpen, onData, peers, connect, send)
import Lib.React (cn, onChange)
import Partial.Unsafe (unsafePartial)
import Prelude hiding (div)
import React (ReactClass, ReactComponent, ReactElement, ReactThis, component, createLeafElement, getProps, getState, modifyState)
import React.DOM (button, div, input, text, span)
import React.DOM.Props (_type, autoFocus, onClick, placeholder, style, value)
import ReactDOM (render)
import Web.DOM.NonElementParentNode (getElementById)
import Web.File.Url (createObjectURL)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

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

type This = ReactThis Props State

appClass :: ReactClass Props
appClass = component "App" \this -> do
  pure
    { state: 
      { lang: "uk"
      , keyText: \key -> key
      , cards: [ { title: "Hello!", image: Nothing } ]
      , question: ""
      } :: State
    , render: render this
    , componentDidMount: do
        setLang this "uk"
        props <- getProps this
        onConnection props.peer \conn ->
          onOpen conn $ onData conn \x -> case decode x of
            Right (Post card) -> modifyState this \s -> s { cards = card : s.cards }
            _ -> pure unit
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
    state <- getState this
    form <- showForm this
    let cards = showCards state.cards
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
        , onChange \v -> modifyState this _ { question = v }
        ]
      , button
        [ _type "button"
        , onClick \_ -> do
            let peer = props.peer
            let card = { title: state.question, image: Nothing }
            peers peer \ids -> void $ sequence $ ids <#> \id ->
              connect peer id >>= \conn -> onOpen conn $ send conn $ encode $ Post card
            modifyState this \s -> s { cards = card : s.cards, question = "" }
            fetchImage this 0
        ] [ text $ state.keyText "post" ]
      ]

  showCards :: Array Card -> ReactElement
  showCards cards = div [ cn "cards" ] $ cards <#> showCard

  showCard :: Card -> ReactElement
  showCard { title, image: Nothing } =
    div [ cn "card" ] [ showTitle title ]
  showCard { title, image: Just url } =
    div [ cn "card", style { backgroundImage: "url("<>url<>")" } ] [ showTitle title ]

  showTitle :: String -> ReactElement
  showTitle title = span [ cn "card-title" ] [ text title ]

main :: Effect (Maybe ReactComponent)
main = do
  doc <- window >>= document
  container <- getElementById "container" $ toNonElementParentNode doc
  peer <- newPeer { host: "uaapps.xyz", port: 443, secure: true, path: "/board" }
  let props = {
      imagePath: "https://api.api-ninjas.com/v1/randomimage?category=nature&width=500&height=375"
    , imageHeaders:
      [ Accept $ MediaType "image/jpg"
      , RequestHeader "X-Api-Key" "0YJRpHZcyfY185HL1U2cPA==kJYRAKy5BmMcEWHD"
      ]
    , peer: peer
    }
  let element = createLeafElement appClass props
  render element $ unsafePartial $ fromJust container
