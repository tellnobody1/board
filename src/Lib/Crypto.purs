module Lib.Crypto where

import Effect (Effect)
import Web.HTML.Window (Window)

foreign import data Crypto :: Type

foreign import crypto :: Window -> Effect Crypto

foreign import randomUUID :: Crypto -> Effect String
