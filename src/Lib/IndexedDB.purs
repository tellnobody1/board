module Lib.IndexedDB where

import Effect (Effect)
import Prelude (Unit)
import Web.HTML.Window (Window)

foreign import data IDBFactory :: Type

foreign import indexedDB :: Window -> Effect IDBFactory

foreign import data IDBOpenDBRequest :: Type

foreign import open :: String -> IDBFactory -> Effect IDBOpenDBRequest

foreign import onsuccess :: IDBOpenDBRequest -> Effect Unit -> Effect Unit

foreign import data IDBDatabase :: Type

foreign import result :: IDBOpenDBRequest -> Effect IDBDatabase
