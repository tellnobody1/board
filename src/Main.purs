module Main where

import Api (Api(Question, Answer), Card, CardID, CardWithID, Answer, decode, encode)
import Data.Array (take, drop, modifyAt, length, find, foldl, foldr, dropEnd, singleton, (:))
import Data.Either (Either(Right))
import Data.FunctorWithIndex (mapWithIndex)
import Data.Map (Map)
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
import Lib.IndexedDB (IDBDatabase, add, createObjectStore, getAll, indexedDB, objectStore, onsuccess, onsuccess', onupgradeneeded, open, transaction, readonly, result, result', readwrite, getAllKeys, delete, deleteObjectStore)
import Lib.NetworkInformation (connection, downlink, hasConnection)
import Lib.Ninjas (randomImage)
import Lib.Peer (Peer, newPeer, onConnection, onOpen, onData, peers, connect, send)
import Lib.React (cn, onChange, createRoot)
import Lib.React (render) as R
import Partial.Unsafe (unsafePartial)
import Prelude (Unit, show, bind, discard, identity, mempty, pure, unit, void, ($), (*>), (-), (<#>), (<$>), (<>), (=<<), (==), (>>=), (<=), (>), (&&), (>>>), (<<<), (/=))
import Proto.Uint8Array (Uint8Array, newUint8Array)
import React (ReactClass, ReactElement, ReactThis, component, createLeafElement, getProps, getState, modifyState)
import React.DOM (h1, button, div, input, text, span, ol, li)
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

type Feed = String

type Store =
  { add :: Feed -> Uint8Array -> Effect Unit
  , all :: Feed -> (Array Api -> Effect Unit) -> Effect Unit
  }

type State =
  { lang :: String
  , t :: String -> String
  , cards :: Array CardWithID
  , question :: String
  , answer :: String
  , answers :: Answers
  , nav :: Nav
  }

type Answers = Map CardID (Array String)

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
    , answers: Map.empty
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
  props.store.all "questions" \xs -> do
    let cards = foldl (\acc -> case _ of
          Question a -> a : acc
          _ -> acc) [] xs
    modifyState this _ { cards = cards }
    restoreNav this
    fetchImages this cards
  props.store.all "answers" \xs -> do
    let answers = foldl (\acc -> case _ of
          Answer { cardID, answer } -> addAnswers acc cardID answer
          _ -> acc) Map.empty xs
    modifyState this _ { answers = answers }

receiveCard :: This -> Effect Unit
receiveCard this = do
  props <- getProps this
  onConnection props.peer \conn ->
    onOpen conn $ onData conn \x -> case decode x of
      Right (Question cardWithID) -> do
        props.store.add "questions" x
        modifyState this \s -> s { cards = cardWithID : s.cards }
        fetchImage this 0
      Right (Answer { cardID, answer }) -> do
        props.store.add "answers" x
        modifyState this \s -> s { answers = addAnswers s.answers cardID answer }
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

addAnswers :: Answers -> CardID -> Answer -> Answers
addAnswers answers cardID answer = Map.alter (case _ of
  Just xs -> Just $ answer : xs
  Nothing -> Just $ singleton answer) cardID answers

broadcast :: Peer -> Uint8Array -> Effect Unit
broadcast peer msg = peers peer \ids -> void $ sequence $ ids <#> \id -> connect peer id >>= \conn -> onOpen conn $ send conn msg

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
showComments this { cardID, card } = do
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
  openReq <- open "board" 2 =<< indexedDB =<< window
  onupgradeneeded openReq \version -> do
    res <- result' openReq
    if version == 1 then deleteObjectStore "cards" res else pure unit
    createObjectStore "questions" =<< result' openReq
    createObjectStore "answers" =<< result' openReq
  onsuccess' openReq do
    db <- result' openReq
    purgeCards db
    let store =
          { add: \feed x -> add x =<< objectStore feed =<< transaction readwrite feed db
          , all: \feed f -> do
              readReq <- getAll =<< objectStore feed =<< transaction readonly feed db
              onsuccess readReq do
                xs <- result readReq
                f $ foldr (decode >>> case _ of
                  Right a -> (:) a
                  _ -> identity) [] xs
          }
    peer <- newPeer { host: "uaapps.xyz", port: 443, secure: true, path: "/board" }
    root <- (body =<< document =<< window) <#> unsafePartial fromJust <#> toElement >>= createRoot
    R.render root $ createLeafElement appClass { peer, store }

purgeCards :: IDBDatabase -> Effect Unit
purgeCards db = do
  keys <- getAllKeys =<< objectStore "questions" =<< transaction readonly "questions" db
  onsuccess keys do
    xs <- result keys <#> dropEnd 100
    writeStore <- objectStore "questions" =<< transaction readwrite "questions" db
    void $ sequence $ delete writeStore <$> xs
