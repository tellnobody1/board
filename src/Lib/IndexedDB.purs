module Lib.IndexedDB where

import Effect (Effect)
import Prelude (Unit, (>>>))
import Proto.Uint8Array (Uint8Array)
import Unsafe.Coerce (unsafeCoerce)
import Web.HTML.Window (Window)

foreign import data IDBFactory :: Type

foreign import indexedDB :: Window -> Effect IDBFactory

foreign import data IDBRequest :: Type -> Type

foreign import data IDBOpenDBRequest :: Type -> Type

toIDBRequest :: forall a. IDBOpenDBRequest a -> IDBRequest a
toIDBRequest = unsafeCoerce

foreign import open :: String -> IDBFactory -> Effect (IDBOpenDBRequest IDBDatabase)

foreign import onupgradeneeded :: IDBOpenDBRequest IDBDatabase -> Effect Unit -> Effect Unit

foreign import onsuccess :: forall a. IDBRequest a -> Effect Unit -> Effect Unit

onsuccess' :: forall a. IDBOpenDBRequest a -> Effect Unit -> Effect Unit
onsuccess' a b = onsuccess (toIDBRequest a) b

foreign import data IDBDatabase :: Type

foreign import result :: forall a. IDBRequest a -> Effect a

result' :: forall a. IDBOpenDBRequest a -> Effect a
result' = toIDBRequest >>> result

foreign import createObjectStore :: String -> IDBDatabase -> Effect Unit

foreign import data ReadOnly :: Type
foreign import data ReadWrite :: Type

foreign import data IDBTransaction :: Type -> Type

foreign import transaction :: forall a. String -> String -> IDBDatabase -> Effect (IDBTransaction a)

readTransaction :: String -> IDBDatabase -> Effect (IDBTransaction ReadOnly)
readTransaction name db = transaction name "readonly" db

writeTransaction :: String -> IDBDatabase -> Effect (IDBTransaction ReadWrite)
writeTransaction name db = transaction name "readwrite" db

foreign import data IDBObjectStore :: Type -> Type

foreign import objectStore :: forall a. String -> IDBTransaction a -> Effect (IDBObjectStore a)

foreign import add :: Uint8Array -> IDBObjectStore ReadWrite -> Effect Unit

foreign import getAll :: IDBObjectStore ReadOnly -> Effect (IDBRequest (Array Uint8Array))
