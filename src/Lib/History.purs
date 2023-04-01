module Lib.History where

import Control.Monad.Except (runExcept)
import Data.Array (drop)
import Data.Either (Either)
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (Maybe(Just, Nothing))
import Data.String.Common (split)
import Data.String.Pattern (Pattern(..))
import Effect (Effect)
import Foreign (Foreign, ForeignError, unsafeToForeign, readString)
import Prelude (Unit, bind, pure, unit, map, (>>=), ($), (<#>))
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.Event.PopStateEvent as PopStateEvent
import Web.HTML.Event.PopStateEvent.EventTypes (popstate)
import Web.HTML.History (DocumentTitle(..), URL(..))
import Web.HTML.History as H
import Web.HTML.Location (pathname)
import Web.HTML.Window (history, toEventTarget, location)

pushState :: String -> String -> Effect Unit
pushState s url = window >>= history >>= H.pushState (unsafeToForeign s) (DocumentTitle "") (URL url)

replaceState :: Foreign -> String -> Effect Unit
replaceState s url = window >>= history >>= H.replaceState s (DocumentTitle "") (URL url)

addPopstateListener :: (Either (NonEmptyList ForeignError) String -> Effect Unit) -> Effect Unit
addPopstateListener f = do
  popstateListener <- eventListener \event -> do
    case PopStateEvent.fromEvent event of
      Just e -> do
        let s = PopStateEvent.state e
        f $ runExcept $ readString s
      Nothing -> pure unit
  (map toEventTarget window) >>= addEventListener popstate popstateListener false

pathnames :: Effect (Array String)
pathnames = window >>= location >>= pathname <#> split (Pattern "/") <#> drop 1
