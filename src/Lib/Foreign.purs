module Lib.Foreign where

import Foreign (Foreign)

foreign import nullValue :: Foreign

null :: Foreign
null = nullValue
