module Lib.IndexedDB where

import Effect (Effect)
import Foreign (Foreign)
import Prelude (Unit, class Show, show, (>>>), ($))
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

newtype ReadOnly = ReadOnly String
newtype ReadWrite = ReadWrite String

instance showReadOnly :: Show ReadOnly where show (ReadOnly a) = a
instance showReadWrite :: Show ReadWrite where show (ReadWrite a) = a

readonly :: ReadOnly
readonly = ReadOnly "readonly"

readwrite :: ReadWrite
readwrite = ReadWrite "readwrite"

foreign import data IDBTransaction :: Type -> Type

foreign import transaction_ :: forall a. String -> String -> IDBDatabase -> Effect (IDBTransaction a)

transaction :: forall a. Show a => a -> String -> IDBDatabase -> Effect (IDBTransaction a)
transaction mode = transaction_ $ show mode

foreign import data IDBObjectStore :: Type -> Type

foreign import objectStore :: forall a. String -> IDBTransaction a -> Effect (IDBObjectStore a)

foreign import add :: Uint8Array -> IDBObjectStore ReadWrite -> Effect Unit

foreign import getAll :: IDBObjectStore ReadOnly -> Effect (IDBRequest (Array Uint8Array))

newtype Key = Key Foreign

foreign import getAllKeys :: IDBObjectStore ReadOnly -> Effect (IDBRequest (Array Key))

foreign import delete_ :: IDBObjectStore ReadWrite -> Key -> Effect Unit

delete :: IDBObjectStore ReadWrite -> Key -> Effect Unit
delete = delete_
