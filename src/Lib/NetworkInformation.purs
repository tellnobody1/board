module Lib.NetworkInformation where

import Effect (Effect)
import Web.HTML.Navigator (Navigator)

foreign import data NetworkInformation :: Type

foreign import hasConnection :: Navigator -> Effect Boolean

foreign import connection :: Navigator -> Effect NetworkInformation

foreign import downlink :: NetworkInformation -> Effect Number
