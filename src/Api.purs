module Api where

import Control.Monad.Rec.Class (Step(Loop, Done), tailRecM3)
import Data.Either (Either(Left))
import Data.Int.Bits (shl, zshr, (.&.))
import Data.Maybe (Maybe(Just, Nothing))
import Prelude (map, bind, pure, (+), (<), ($))
import Proto.Decode as Decode
import Proto.Encode as Encode
import Proto.Uint8Array (Uint8Array, length, concatAll)

data Api = Post Card

type Card =
  { title :: String
  , image :: Maybe String
  }

type Card' =
  { title :: Maybe String
  }

encode :: Api -> Uint8Array
encode (Post x) = concatAll [ Encode.unsignedVarint32 $ shl 1 3 + 2, encodePost x ]

  where
  
  encodePost :: Card -> Uint8Array
  encodePost msg = do
    let xs = concatAll
          [ Encode.unsignedVarint32 $ shl 1 3 + 2
          , Encode.string msg.title
          ]
    concatAll [ Encode.unsignedVarint32 $ length xs, xs ]

decode :: Uint8Array -> Either Decode.Error Api
decode _xs_ = do
  { pos: pos1, val: tag } <- Decode.unsignedVarint32 _xs_ 0
  case tag `zshr` 3 of
    1 -> map Post $ decodePost pos1
    i -> Left $ Decode.BadType i

  where

  decodePost :: Int -> Either Decode.Error Card
  decodePost pos0 = do
    { pos, val: msglen } <- Decode.unsignedVarint32 _xs_ pos0
    { val } <- tailRecM3 decodePost_ (pos + msglen) { title: Nothing } pos
    case val of
      { title: Just title } -> pure { title, image: Nothing }
      _ -> Left $ Decode.MissingFields "Card"
      
    where
    
    decodePost_ :: Int -> Card' -> Int -> Decode.Result' (Step { a :: Int, b :: Card', c :: Int } { pos :: Int, val :: Card' })
    decodePost_ end acc pos1 | pos1 < end = do
      { pos: pos2, val: tag } <- Decode.unsignedVarint32 _xs_ pos1
      case tag `zshr` 3 of
        1 -> decodeFieldLoop end (Decode.string _xs_ pos2) \val -> acc { title = Just val }
        _ -> decodeFieldLoop end (Decode.skipType _xs_ pos2 $ tag .&. 7) \_ -> acc
    decodePost_ _ acc pos1 = pure $ Done { pos: pos1, val: acc }

decodeFieldLoop :: forall a b c. Int -> Decode.Result a -> (a -> b) -> Decode.Result' (Step { a :: Int, b :: b, c :: Int } { pos :: Int, val :: c })
decodeFieldLoop end res f = map (\{ pos, val } -> Loop { a: end, b: f val, c: pos }) res