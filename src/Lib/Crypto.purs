module Lib.Crypto where

import Effect (Effect)
import Prelude (Unit)
import Proto.Uint8Array (Uint8Array)
import Web.HTML.Window (Window)

foreign import data Crypto :: Type

foreign import crypto :: Window -> Effect Crypto

foreign import randomUUID :: Crypto -> Effect String

foreign import getRandomValues :: Uint8Array -> Crypto -> Effect Unit
