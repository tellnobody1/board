module Main where

import Answer (addAnswers, answersPage)
import Api (Api(Answer, Question), QuestionCardWithID, decode)
import Data.Array (drop, dropEnd, find, foldl, foldr, length, take, (:))
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
import Lib.Foreign (null)
import Lib.History (addPopstateListener, pathnames, replaceState)
import Lib.IndexedDB (IDBDatabase, add, createObjectStore, getAll, indexedDB, objectStore, onsuccess, onsuccess', onupgradeneeded, open, transaction, readonly, result, result', readwrite, getAllKeys, delete, deleteObjectStore)
import Lib.Peer (newPeer, onConnection, onData, onOpen)
import Lib.React (createRoot, render)
import Partial.Unsafe (unsafePartial)
import Prelude (Unit, bind, discard, identity, mempty, pure, unit, void, ($), (*>), (-), (<#>), (<$>), (<>), (=<<), (==), (>>=), (>>>))
import Question (questionForm, fetchImage, questionCards)
import React (ReactClass, ReactElement, component, createLeafElement, getProps, getState, modifyState)
import React.DOM (div)
import Types
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.Event.EventTypes (domcontentloaded)
import Web.HTML.HTMLDocument (readyState, body)
import Web.HTML.HTMLDocument.ReadyState (ReadyState(..))
import Web.HTML.HTMLElement (toElement)
import Web.HTML.Window (document, toEventTarget)

appClass :: ReactClass Props
appClass = component "App" \this -> pure
  { state: 
    { lang: ""
    , t: identity
    , questions: []
    , question: ""
    , answer: ""
    , answers: Map.empty
    , nav: EmptyView
    }
  , render: renderApp this
  , componentDidMount: do
      setLang this "uk"
      addPopstateListener' this
      receiveData this
      restoreState this
  }

goHome :: This -> Effect Unit
goHome this = do
  replaceState null "/"
  modifyState this _ { nav = ViewCards }

addPopstateListener' :: This -> Effect Unit
addPopstateListener' this = addPopstateListener $ case _ of
  Right questionID -> modifyState this _ { nav = ViewCard questionID }
  _ -> modifyState this _ { nav = ViewCards }

restoreNav :: This -> Effect Unit
restoreNav this = pathnames >>= case _ of
  [ "post", questionID ] -> modifyState this _ { nav = ViewCard questionID }
  _ -> goHome this

restoreState :: This -> Effect Unit
restoreState this = do
  props <- getProps this
  props.store.all "questions" \xs -> do
    let questions = foldl (\acc -> case _ of
          Question a -> a : acc
          _ -> acc) [] xs
    modifyState this _ { questions = questions }
    restoreNav this
    fetchImages this questions
  props.store.all "answers" \xs -> do
    let answers = foldl (\acc -> case _ of
          Answer { questionID, answer } -> addAnswers acc questionID answer
          _ -> acc) Map.empty xs
    modifyState this _ { answers = answers }

receiveData :: This -> Effect Unit
receiveData this = do
  props <- getProps this
  onConnection props.peer \conn ->
    onOpen conn $ onData conn \x -> case decode x of
      Right (Question questionCardWithID) -> do
        props.store.add "questions" x
        modifyState this \s -> s { questions = questionCardWithID : s.questions }
        fetchImage this 0
      Right (Answer { questionID, answer }) -> do
        props.store.add "answers" x
        modifyState this \s -> s { answers = addAnswers s.answers questionID answer }
      _ -> pure unit

setLang :: This -> String -> Effect Unit
setLang this lang = do
  getEff ("/langs/"<>lang<>".js") \v -> do
    let keys = Map.fromFoldable $ split (Pattern "\n") v <#> split (Pattern "=") <#> \kv -> Tuple (joinWith "" $ take 1 kv) (joinWith "" $ drop 1 kv)
    modifyState this _ { lang = lang, t = \key -> fromMaybe key $ Map.lookup key keys }

fetchImages :: This -> Array QuestionCardWithID -> Effect Unit
fetchImages this questions = void $ sequence $ mapWithIndex (\i _ -> fetchImage this $ length questions - i - 1) questions

renderApp :: This -> Effect ReactElement
renderApp this = do
  state <- getState this
  case state.nav of
    EmptyView -> pure mempty
    ViewCards -> do
      form <- questionForm this
      questions <- questionCards this
      pure $
        div []
        [ form
        , questions
        ]
    ViewCard questionID ->
      case find (\x -> x.questionID == questionID) state.questions of
        Just question -> answersPage this question
        Nothing -> goHome this *> mempty

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
    if version == 1 then deleteObjectStore "questions" res else pure unit
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
    render root $ createLeafElement appClass { peer, store }

purgeCards :: IDBDatabase -> Effect Unit
purgeCards db = do
  keys <- getAllKeys =<< objectStore "questions" =<< transaction readonly "questions" db
  onsuccess keys do
    xs <- result keys <#> dropEnd 100
    writeStore <- objectStore "questions" =<< transaction readwrite "questions" db
    void $ sequence $ delete writeStore <$> xs
