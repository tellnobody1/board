module App where

import Prelude hiding (div)

import Data.Array (take, drop)
import Data.Foldable (find)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.String.Common (joinWith, split, toLower, trim, null)
import Data.String.Pattern (Pattern(Pattern))
import Data.Tuple (Tuple(Tuple))
import Effect (Effect)
import Effect.Class.Console (infoShow)
import Effect.Exception (throw)
import Lib.Ajax (getEff)
import Lib.React (cn, onChangeValue)
import React (ReactClass, ReactElement, ReactThis, component, createLeafElement, getProps, getState, modifyState)
import React.DOM (a, button, div, img, li, nav, option, select, span, text, ul)
import React.DOM.Dynamic (menuitem)
import React.DOM.Props (_type, height, href, onClick, src, style, value, _id)
import ReactDOM (render)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Location (protocol, hostname)
import Web.HTML.Window (document, location)

type Props = {}

type State =
  { lang :: String
  , keyText :: String -> String
  }

type This = ReactThis Props State

appClass :: ReactClass Props
appClass = component "App" \this -> do
  pure
    { state: 
      { lang: "uk"
      , keyText: \key -> key
      } :: State
    , render: render this
    , componentDidMount: setLang this "uk"
    }
  where

  setLang :: This -> String -> Effect Unit
  setLang this lang = do
    getEff ("langs/" <> lang <> ".js") \v -> do
      let keys = Map.fromFoldable $ split (Pattern "\n") v <#> split (Pattern "=") <#> \kv -> Tuple (joinWith "" $ take 1 kv) (joinWith "" $ drop 1 kv)
      modifyState this _ { lang = lang, keyText = \key -> fromMaybe key $ Map.lookup (toLower key) keys }

  render :: This -> Effect ReactElement
  render this = do
    state <- getState this
    pure $ 
      div []
      [ text $ state.keyText "uk"
      ]

view :: Effect Unit
view = do
  doc <- window >>= document
  elem <- getElementById "container" $ toNonElementParentNode doc
  container <- maybe (throw "container not found") pure elem
  let element = createLeafElement appClass {}
  void $ render element container
