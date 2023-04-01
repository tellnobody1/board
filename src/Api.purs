module Api where

import Control.Monad.Rec.Class (Step(Loop, Done), tailRecM3)
import Data.Either (Either(Left))
import Data.Int.Bits (shl, zshr, (.&.))
import Data.Maybe (Maybe(Just, Nothing))
import Prelude (map, bind, pure, (+), (<), ($))
import Proto.Decode as Decode
import Proto.Encode as Encode
import Proto.Uint8Array (Uint8Array, length, concatAll)

data Api = Post CardWithID

type CardID = String

type Card =
  { title :: String
  , image :: Maybe String
  }

type CardWithID = { cardID :: CardID, card :: Card }

type Card' =
  { title :: Maybe String
  }

type CardWithID' = { cardID :: Maybe CardID, card :: Card' }

encode :: Api -> Uint8Array
encode (Post x) = concatAll [ Encode.unsignedVarint32 $ shl 1 3 + 2, encodePost x ]

encodePost :: CardWithID -> Uint8Array
encodePost msg = do
  let xs = concatAll
        [ Encode.unsignedVarint32 $ shl 1 3 + 2
        , Encode.string msg.cardID
        , Encode.unsignedVarint32 $ shl 2 3 + 2
        , Encode.string msg.card.title
        ]
  concatAll [ Encode.unsignedVarint32 $ length xs, xs ]

decode :: Uint8Array -> Either Decode.Error Api
decode _xs_ = do
  { pos: pos1, val: tag } <- Decode.unsignedVarint32 _xs_ 0
  case tag `zshr` 3 of
    1 -> map Post $ decodePost pos1
    i -> Left $ Decode.BadType i

  where

  decodePost :: Int -> Either Decode.Error CardWithID
  decodePost pos0 = do
    { pos, val: msglen } <- Decode.unsignedVarint32 _xs_ pos0
    { val } <- tailRecM3 decodePost_ (pos + msglen) { cardID: Nothing, card: { title: Nothing } } pos
    case val of
      { cardID: Just cardID, card: { title: Just title } } -> pure { cardID, card: { title, image: Nothing } }
      _ -> Left $ Decode.MissingFields "CardWithID"
      
  decodePost_ :: Int -> CardWithID' -> Int -> Decode.Result' (Step { a :: Int, b :: CardWithID', c :: Int } { pos :: Int, val :: CardWithID' })
  decodePost_ end acc pos1 | pos1 < end = do
    { pos: pos2, val: tag } <- Decode.unsignedVarint32 _xs_ pos1
    case tag `zshr` 3 of
      1 -> decodeFieldLoop end (Decode.string _xs_ pos2) \val -> acc { cardID = Just val }
      2 -> decodeFieldLoop end (Decode.string _xs_ pos2) \val -> acc { card = acc.card { title = Just val } }
      _ -> decodeFieldLoop end (Decode.skipType _xs_ pos2 $ tag .&. 7) \_ -> acc
  decodePost_ _ acc pos1 = pure $ Done { pos: pos1, val: acc }

decodeFieldLoop :: forall a b c. Int -> Decode.Result a -> (a -> b) -> Decode.Result' (Step { a :: Int, b :: b, c :: Int } { pos :: Int, val :: c })
decodeFieldLoop end res f = map (\{ pos, val } -> Loop { a: end, b: f val, c: pos }) res