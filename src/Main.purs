module Main where

import Api (Api(Post), Card, CardID, CardWithID, decode, encode)
import Data.Array (take, drop, modifyAt, length, find, foldl, dropEnd, (:))
import Data.Either (Either(Right))
import Data.FunctorWithIndex (mapWithIndex)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, fromJust)
import Data.String.Common (joinWith, split)
import Data.String.Pattern (Pattern(..))
import Data.Traversable (sequence)
import Data.Tuple (Tuple(Tuple))
import Effect (Effect)
import Lib.Affjax (getEff)
import Lib.Array (from)
import Lib.Crypto (crypto, randomUUID, getRandomValues)
import Lib.Foreign (null)
import Lib.History (pushState, replaceState, addPopstateListener, pathnames)
import Lib.IndexedDB (IDBDatabase, add, createObjectStore, getAll, indexedDB, objectStore, onsuccess, onsuccess', onupgradeneeded, open, transaction, readonly, result, result', readwrite, getAllKeys, delete)
import Lib.NetworkInformation (connection, downlink)
import Lib.Ninjas (randomImage)
import Lib.Peer (Peer, newPeer, onConnection, onOpen, onData, peers, connect, send)
import Lib.React (cn, onChange, createRoot)
import Lib.React (render) as R
import Partial.Unsafe (unsafePartial)
import Prelude (Unit, show, bind, discard, identity, mempty, pure, unit, void, ($), (*>), (-), (<#>), (<$>), (<>), (=<<), (==), (>>=), (<=), (>), (&&))
import Proto.Uint8Array (Uint8Array, newUint8Array)
import React (ReactClass, ReactElement, ReactThis, component, createLeafElement, getProps, getState, modifyState)
import React.DOM (h1, button, div, input, text, span)
import React.DOM.Props (_type, autoFocus, onClick, placeholder, style, value)
import React.DOM.Props (Props) as R
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.Event.EventTypes (domcontentloaded)
import Web.HTML.HTMLDocument (setTitle, readyState, body)
import Web.HTML.HTMLDocument.ReadyState (ReadyState(..))
import Web.HTML.HTMLElement (toElement)
import Web.HTML.Window (document, navigator, toEventTarget)

type Props =
  { peer :: Peer
  , store :: Store
  }

type Store =
  { add :: Uint8Array -> Effect Unit
  , all :: (Array CardWithID -> Effect Unit) -> Effect Unit
  }

type State =
  { lang :: String
  , t :: String -> String
  , cards :: Array CardWithID
  , question :: String
  , answer :: String
  , nav :: Nav
  }

data Nav = EmptyView | ViewCards | ViewCard CardID

type This = ReactThis Props State

appClass :: ReactClass Props
appClass = component "App" \this -> pure
  { state: 
    { lang: ""
    , t: identity
    , cards: []
    , question: ""
    , answer: ""
    , nav: EmptyView
    }
  , render: render this
  , componentDidMount: do
      setLang this "uk"
      addPopstateListener' this
      receiveCard this
      restoreState this
  }

goHome :: This -> Effect Unit
goHome this = do
  replaceState null "/"
  modifyState this _ { nav = ViewCards }

goToCard :: This -> CardID -> Card -> Effect Unit
goToCard this cardID card = do
  pushState cardID $ "/post/"<>cardID
  window >>= document >>= setTitle card.title
  modifyState this _ { nav = ViewCard cardID }

addPopstateListener' :: This -> Effect Unit
addPopstateListener' this = addPopstateListener $ case _ of
  Right cardID -> modifyState this _ { nav = ViewCard cardID }
  _ -> modifyState this _ { nav = ViewCards }

restoreNav :: This -> Effect Unit
restoreNav this = pathnames >>= case _ of
  [ "post", cardID ] -> modifyState this _ { nav = ViewCard cardID }
  _ -> goHome this

restoreState :: This -> Effect Unit
restoreState this = do
  props <- getProps this
  props.store.all \cards -> do
    modifyState this _ { cards = cards }
    restoreNav this
    fetchImages this cards

receiveCard :: This -> Effect Unit
receiveCard this = do
  props <- getProps this
  onConnection props.peer \conn ->
    onOpen conn $ onData conn \x -> case decode x of
      Right (Post cardWithID) -> do
        props.store.add x
        modifyState this \s -> s { cards = cardWithID : s.cards }
      _ -> pure unit

setLang :: This -> String -> Effect Unit
setLang this lang = do
  getEff ("/langs/"<>lang<>".js") \v -> do
    let keys = Map.fromFoldable $ split (Pattern "\n") v <#> split (Pattern "=") <#> \kv -> Tuple (joinWith "" $ take 1 kv) (joinWith "" $ drop 1 kv)
    modifyState this _ { lang = lang, t = \key -> fromMaybe key $ Map.lookup key keys }

fetchImages :: This -> Array CardWithID -> Effect Unit
fetchImages this cards = void $ sequence $ mapWithIndex (\i _ -> fetchImage this $ length cards - i - 1) cards

fetchImage :: This -> Int -> Effect Unit
fetchImage this i = do
  speed <- downlink =<< connection =<< navigator =<< window
  if i <= 2 && speed > 5.0 then do
    randomImage \url -> setImage $ "url(" <> url <> ")"
  else do
    let xs = newUint8Array 3
    getRandomValues xs =<< crypto =<< window
    setImage $ "rgba(" <> joinWith ", " (show <$> from xs) <> ", 0.3)"

  where

  setImage :: String -> Effect Unit
  setImage background = modifyState this \s -> s { cards = fromMaybe s.cards (modifyAt i (\x -> x { card = x.card { background = Just background } }) s.cards) }

render :: This -> Effect ReactElement
render this = do
  state <- getState this
  case state.nav of
    EmptyView -> pure mempty
    ViewCards -> do
      form <- showForm this
      cards <- showCards this
      pure $
        div []
        [ form
        , cards
        ]
    ViewCard cardID ->
      case find (\x -> x.cardID == cardID) state.cards of
        Just card -> showComments this card
        Nothing -> goHome this *> mempty

showForm :: This -> Effect ReactElement
showForm this = do
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
          props <- getProps this
          let peer = props.peer
          cardID <- randomUUID =<< crypto =<< window
          let card = { title: state.question, background: Nothing }
          let cardWithID = { cardID, card }
          let encoded = encode $ Post cardWithID
          peers peer \ids -> void $ sequence $ ids <#> \id ->
            connect peer id >>= \conn -> onOpen conn $ send conn encoded
          props.store.add encoded
          modifyState this \s -> s { cards = cardWithID : s.cards, question = "" }
          fetchImage this 0
      ] [ text $ state'.t "post" ]
    ]

showCards :: This -> Effect ReactElement
showCards this = do
  state <- getState this
  pure $ div [ cn "cards" ] $ state.cards <#> showCard this

showCard :: This -> CardWithID -> ReactElement
showCard this { cardID, card } =
  div (
  [ cn "card card-link"
  , onClick \_ -> goToCard this cardID card
  ] <> showImage card.background) $ showTitle card

showComments :: This -> CardWithID -> Effect ReactElement
showComments this { cardID: _, card } = do
  state <- getState this
  pure $
    div []
    [ h1 [ cn "card-header" ] [ text card.title ]
    , div [ cn "form" ]
      [ input
        [ _type "text", placeholder $ state.t "answer", autoFocus true
        , value state.answer
        , onChange \v -> modifyState this _ { answer = v }
        ]
      , button
        [ _type "button"
        , onClick \_ -> do
            modifyState this _ { answer = "" }
        ] [ text $ state.t "post" ]
      ]
    ]

showTitle :: Card -> Array ReactElement
showTitle card = [ span [ cn "card-title" ] [ text card.title ] ]

showImage :: Maybe String -> Array R.Props
showImage Nothing = []
showImage (Just background) = [ style { background: background } ]

main :: Effect Unit
main = do
  ready <- readyState =<< document =<< window
  case ready of
    Loading -> do
      listener <- eventListener \_ -> renderClass
      target <- toEventTarget <$> window
      addEventListener domcontentloaded listener false target
    _ -> renderClass

renderClass :: Effect Unit
renderClass = do
  openReq <- open "board" =<< indexedDB =<< window
  onupgradeneeded openReq $ createObjectStore "cards" =<< result' openReq
  onsuccess' openReq do
    db <- result' openReq
    purgeCards db
    let store =
          { add: \x -> add x =<< objectStore "cards" =<< transaction readwrite "cards" db
          , all: \f -> do
              readReq <- getAll =<< objectStore "cards" =<< transaction readonly "cards" db
              onsuccess readReq do
                xs <- result readReq
                f $ foldl (\acc x -> case decode x of
                      Right (Post a) -> a : acc
                      _ -> acc) [] xs
          }
    peer <- newPeer { host: "uaapps.xyz", port: 443, secure: true, path: "/board" }
    root <- (body =<< document =<< window) <#> unsafePartial fromJust <#> toElement >>= createRoot
    R.render root $ createLeafElement appClass { peer, store }

purgeCards :: IDBDatabase -> Effect Unit
purgeCards db = do
  keys <- getAllKeys =<< objectStore "cards" =<< transaction readonly "cards" db
  onsuccess keys do
    xs <- result keys <#> dropEnd 100
    writeStore <- objectStore "cards" =<< transaction readwrite "cards" db
    void $ sequence $ delete writeStore <$> xs
