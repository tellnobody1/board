module Lib.IndexedDB where

import Effect (Effect)
import Prelude (Unit)
import Proto.Uint8Array (Uint8Array)
import Web.HTML.Window (Window)

foreign import data IDBFactory :: Type

foreign import indexedDB :: Window -> Effect IDBFactory

foreign import data IDBOpenDBRequest :: Type

foreign import open :: String -> IDBFactory -> Effect IDBOpenDBRequest

foreign import onupgradeneeded :: IDBOpenDBRequest -> Effect Unit -> Effect Unit

foreign import onsuccess :: IDBOpenDBRequest -> Effect Unit -> Effect Unit

foreign import data IDBDatabase :: Type

foreign import result :: IDBOpenDBRequest -> Effect IDBDatabase

foreign import createObjectStore :: String -> IDBDatabase -> Effect Unit

foreign import data IDBTransaction :: Type

foreign import transaction :: String -> String -> IDBDatabase -> Effect IDBTransaction

readTransaction :: String -> IDBDatabase -> Effect IDBTransaction
readTransaction name db = transaction name "readonly" db

writeTransaction :: String -> IDBDatabase -> Effect IDBTransaction
writeTransaction name db = transaction name "readwrite" db

foreign import data IDBObjectStore :: Type

foreign import objectStore :: String -> IDBTransaction -> Effect IDBObjectStore

foreign import add :: Uint8Array -> IDBObjectStore -> Effect Unit
