module Lib.Crypto where

import Effect (Effect)

foreign import data Crypto :: Type

foreign import crypto_ :: Effect Crypto

crypto :: Effect Crypto
crypto = crypto_

foreign import randomUUID :: Crypto -> Effect String
