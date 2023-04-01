module Main where

import Api (Api(Post), Card, CardID, CardWithID, decode, encode)
import Data.Array (take, drop, modifyAt, length, find, (:))
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
import Lib.Crypto (crypto, randomUUID)
import Lib.Foreign (null)
import Lib.History (pushState, replaceState, addPopstateListener, pathnames)
import Lib.Ninjas (randomImage)
import Lib.Peer (Peer, newPeer, onConnection, onOpen, onData, peers, connect, send)
import Lib.React (cn, onChange, createRoot, render)
import Partial.Unsafe (unsafePartial)
import Prelude hiding (div)
import React (ReactClass, ReactElement, ReactThis, component, createLeafElement, getProps, getState, modifyState)
import React.DOM (button, div, input, text, span)
import React.DOM.Props (_type, autoFocus, onClick, placeholder, style, value)
import React.DOM.Props as R
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode, setTitle)
import Web.HTML.Window (document)

type Props = { peer :: Peer }

type State =
  { lang :: String
  , t :: String -> String
  , cards :: Array CardWithID
  , question :: String
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
    , nav: EmptyView
    } :: State
  , render: render this
  , componentDidMount: do
      setLang this "uk"
      addPopstateListener' this
      receiveCard this
      fetchImages this
      restoreState this
  }

  where

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

  restoreState :: This -> Effect Unit
  restoreState this = pathnames >>= case _ of
    [ "post", cardID ] -> modifyState this _ { nav = ViewCard cardID }
    _ -> goHome this

  receiveCard :: This -> Effect Unit
  receiveCard this = do
    props <- getProps this
    onConnection props.peer \conn ->
      onOpen conn $ onData conn \x -> case decode x of
        Right (Post cardWithID) -> modifyState this \s -> s { cards = cardWithID : s.cards }
        _ -> pure unit

  setLang :: This -> String -> Effect Unit
  setLang this lang = do
    getEff ("langs/"<>lang<>".js") \v -> do
      let keys = Map.fromFoldable $ split (Pattern "\n") v <#> split (Pattern "=") <#> \kv -> Tuple (joinWith "" $ take 1 kv) (joinWith "" $ drop 1 kv)
      modifyState this _ { lang = lang, t = \key -> fromMaybe key $ Map.lookup key keys }

  fetchImages :: This -> Effect Unit
  fetchImages this = do
    state <- getState this
    let cards = state.cards
    void $ sequence $ mapWithIndex (\i _ -> fetchImage this $ length cards - i - 1) cards

  fetchImage :: This -> Int -> Effect Unit
  fetchImage this i = randomImage \url ->
    modifyState this \s -> s { cards = fromMaybe s.cards (modifyAt i (\x -> x { card = x.card { image = Just url } }) s.cards) }

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
          Just card -> pure $ showCardAndComments this card
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
            cardID <- crypto >>= randomUUID
            let card = { title: state.question, image: Nothing }
            let cardWithID = { cardID, card }
            peers peer \ids -> void $ sequence $ ids <#> \id ->
              connect peer id >>= \conn -> onOpen conn $ send conn $ encode $ Post cardWithID
            modifyState this \s -> s { cards = cardWithID : s.cards, question = "" }
            fetchImage this 0
        ] [ text $ state'.t "post" ]
      ]

  showCards :: This -> Effect ReactElement
  showCards this = do
    state <- getState this
    pure $ div [ cn "cards" ] $ state.cards <#> showCard

    where

    showCard :: CardWithID -> ReactElement
    showCard { cardID, card } =
      div (
      [ cn "card"
      , onClick \_ -> goToCard this cardID card
      ] <> showImage card.image) $ showTitle card

  showCardAndComments :: This -> CardWithID -> ReactElement
  showCardAndComments _ { cardID: _, card } =
    div ([ cn "card" ] <> showImage card.image) $ showTitle card

  showTitle :: Card -> Array ReactElement
  showTitle card = [ span [ cn "card-title" ] [ text card.title ] ]

  showImage :: Maybe String -> Array R.Props
  showImage Nothing = []
  showImage (Just url) = [ style { backgroundImage: "url("<>url<>")" } ]

main :: Effect Unit
main = do
  doc <- window >>= document
  container <- getElementById "container" $ toNonElementParentNode doc
  peer <- newPeer { host: "uaapps.xyz", port: 443, secure: true, path: "/board" }
  let props = { peer }
  root <- createRoot $ unsafePartial $ fromJust container
  render root $ createLeafElement appClass props
