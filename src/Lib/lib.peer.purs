module Lib.Peer where

import Prelude
import Effect (Effect)

foreign import data Peer :: Type

type Host = String
type Port = Int
type Path = String
type Data = String

foreign import initPeer :: Host -> Port -> Path -> Effect Peer

foreign import onData :: Peer -> (Data -> Effect Unit) -> Effect Unit

foreign import sendData :: Peer -> Data -> Effect Unit
