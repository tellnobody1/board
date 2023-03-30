module Lib.Peer where

import Prelude
import Affjax.ResponseFormat (string)
import Affjax.StatusCode (StatusCode(StatusCode))
import Affjax.Web (get)
import Data.Array (filter)
import Data.Either (Either(Left, Right))
import Data.Traversable (sequence)
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

foreign import initPeer :: Options -> Effect Peer

foreign import onData :: Peer -> (Data -> Effect Unit) -> Effect Unit

foreign import connect :: Peer -> ID -> Effect Connection

foreign import onOpen :: Connection -> Effect Unit -> Effect Unit

foreign import send :: Connection -> Data -> Effect Unit

broadcast :: Peer -> Data -> Effect Unit
broadcast peer data_ =
  runAff_ (case _ of
    Right (Right a) -> case readJSON a of
      Right ids -> void $
        sequence $
        map (\id -> connect peer id >>= \conn -> onOpen conn $ send conn data_) $
        filter (_ /= peer.id) ids
      _ -> pure unit
    _ -> pure unit
  ) $ map (case _ of
    Right { status: StatusCode 200, body } -> Right body
    _ -> Left unit) $ get string (peersUrl peer)

peersUrl :: Peer -> String
peersUrl peer = do
  let opt = peer.options
  "https://" <> opt.host <> ":" <> (show opt.port) <> opt.path <> "peerjs/peers"
