module Lib.Peer where

import Prelude
import Affjax.ResponseFormat (string)
import Affjax.StatusCode (StatusCode(StatusCode))
import Affjax.Web (get)
import Data.Array (filter)
import Data.Either (Either(Left, Right))
import Effect (Effect)
import Effect.Aff (runAff_)
import Simple.JSON (readJSON)

type ID = String
type Host = String
type Port = Int
type Path = String
type Data = String

type Options =
  { host :: Host
  , port :: Port
  , secure :: Boolean
  , path :: Path
  }

type Peer =
  { id :: ID
  , options :: Options
  }

foreign import data Connection :: Type

foreign import newPeer :: Options -> Effect Peer

foreign import onConnection :: Peer -> (Connection -> Effect Unit) -> Effect Unit

foreign import onData :: Connection -> (Data -> Effect Unit) -> Effect Unit

foreign import connect :: Peer -> ID -> Effect Connection

foreign import onOpen :: Connection -> Effect Unit -> Effect Unit

foreign import send :: Connection -> Data -> Effect Unit

peers :: Peer -> (Array ID -> Effect Unit) -> Effect Unit
peers peer f =
  runAff_ (case _ of
    Right (Right body) -> case readJSON body of
      Right ids -> f $ filter (_ /= peer.id) ids
      _ -> pure unit
    _ -> pure unit
  ) $ map (case _ of
    Right { status: StatusCode 200, body } -> Right body
    _ -> Left unit) $ get string (peersUrl peer)

peersUrl :: Peer -> String
peersUrl peer = do
  let opt = peer.options
  "https://" <> opt.host <> ":" <> (show opt.port) <> opt.path <> "peerjs/peers"
