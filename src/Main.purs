module Main where

import Prelude hiding (div)

import Affjax.RequestHeader (RequestHeader(..))
import Data.Array (take, drop, modifyAt)
import Data.Foldable (find)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.MediaType (MediaType(..))
import Data.String.Common (joinWith, split, toLower, trim, null)
import Data.String.Pattern (Pattern(Pattern))
import Data.Traversable (sequence)
import Data.Tuple (Tuple(Tuple))
import Effect (Effect)
import Effect.Class.Console (infoShow)
import Effect.Exception (throw)
import Lib.Ajax (getEff, getBlobEff)
import Lib.React (cn, onChangeValue)
import React (ReactClass, ReactElement, ReactThis, component, createLeafElement, getProps, getState, modifyState)
import React.DOM (a, button, div, img, li, nav, option, select, span, text, ul, input, h5)
import React.DOM.Dynamic (menuitem)
import React.DOM.Props (_type, height, href, onClick, src, style, value, _id, placeholder, autoFocus)
import ReactDOM (render)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Location (protocol, hostname)
import Web.HTML.Window (document, location)
import Web.File.Url (createObjectURL)

type Props =
  { apiKey :: String
  }

type State =
  { lang :: String
  , keyText :: String -> String
  , cards :: Array Card
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
      } :: State
    , render: render this
    , componentDidMount: do
        setLang this "uk"
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
    getBlobEff "https://api.api-ninjas.com/v1/randomimage?category=nature&width=500&height=375" [ Accept $ MediaType "image/jpg", RequestHeader "X-Api-Key" props.apiKey ] \v -> do
      url <- createObjectURL v
      modifyState this \s -> s { cards = fromMaybe s.cards (modifyAt index (_ { image = Just url }) s.cards) }

  render :: This -> Effect ReactElement
  render this = do
    form <- showForm this
    cards <- showCards this
    pure $ 
      div [ cn "container mt-4" ]
      [ form
      , cards
      ]

  showForm :: This -> Effect ReactElement
  showForm this = do
    state <- getState this
    pure $
      div [ cn "row" ]
      [ div [ cn "col" ]
        [ div [ cn "input-group" ]
          [ input [ _type "text", cn "form-control", placeholder $ state.keyText "question", autoFocus true ]
          , button [ _type "button", cn "btn btn-primary" ] [ text $ state.keyText "post" ]
          ]
        ]
      ]

  showCards :: This -> Effect ReactElement
  showCards this = do
    state <- getState this
    rows <- sequence $ map (showCard this) state.cards
    pure $
      div [ cn "row row-cols-1 row-cols-md-3 g-4 mt-0" ] rows

  showCard :: This -> Card -> Effect ReactElement
  showCard _ card = do
    pure $
      div [ cn "col" ]
      [ div [ cn "card" ]
        [ showImage card.image
        , div [ cn "card-img-overlay" ]
          [ h5 [ cn "card-title bg-dark d-inline px-2 py-1" ] [ text $ card.title ]
          ]
        ]
      ]

  showImage :: Maybe String -> ReactElement
  showImage Nothing = div [ cn "rounded bg-light", style { aspectRatio: "4 / 3" } ] [ ]
  showImage (Just url) = img [ cn "rounded", src url ]

main :: Effect Unit
main = do
  doc <- window >>= document
  elem <- getElementById "container" $ toNonElementParentNode doc
  container <- maybe (throw "container not found") pure elem
  let element = createLeafElement appClass { apiKey: "0YJRpHZcyfY185HL1U2cPA==kJYRAKy5BmMcEWHD" }
  void $ render element container
