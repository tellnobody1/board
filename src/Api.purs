module Api where

import Control.Monad.Rec.Class (Step(Loop, Done), tailRecM3)
import Data.Either (Either(Left))
import Data.Int.Bits (shl, zshr, (.&.))
import Data.Maybe (Maybe(Just, Nothing))
import Prelude (map, bind, pure, (+), (<), ($), (<$>))
import Proto.Decode as Decode
import Proto.Encode as Encode
import Proto.Uint8Array (Uint8Array, length, concatAll)

data Api = Question CardWithID | Answer AnswerWithID

type CardID = String
type CardID' = Maybe String

type Card =
  { title :: String
  , background :: Maybe String
  }
type Answer = String

type CardWithID = { cardID :: CardID, card :: Card }
type AnswerWithID = { cardID :: CardID, answer :: Answer }

type Card' =
  { title :: Maybe String
  }
type Answer' = Maybe String

type CardWithID' = { cardID :: CardID', card :: Card' }
type AnswerWithID' = { cardID :: CardID', answer :: Answer' }

encode :: Api -> Uint8Array
encode (Question x) = concatAll [ Encode.unsignedVarint32 $ shl 1 3 + 2, encodeQuestion x ]
encode (Answer x) = concatAll [ Encode.unsignedVarint32 $ shl 2 3 + 2, encodeAnswer x ]

encodeQuestion :: CardWithID -> Uint8Array
encodeQuestion msg = do
  let xs = concatAll
        [ Encode.unsignedVarint32 $ shl 1 3 + 2
        , Encode.string msg.cardID
        , Encode.unsignedVarint32 $ shl 2 3 + 2
        , Encode.string msg.card.title
        ]
  concatAll [ Encode.unsignedVarint32 $ length xs, xs ]

encodeAnswer :: AnswerWithID -> Uint8Array
encodeAnswer msg = do
  let xs = concatAll
        [ Encode.unsignedVarint32 $ shl 1 3 + 2
        , Encode.string msg.cardID
        , Encode.unsignedVarint32 $ shl 2 3 + 2
        , Encode.string msg.answer
        ]
  concatAll [ Encode.unsignedVarint32 $ length xs, xs ]

decode :: Uint8Array -> Either Decode.Error Api
decode _xs_ = do
  { pos: pos1, val: tag } <- Decode.unsignedVarint32 _xs_ 0
  case tag `zshr` 3 of
    1 -> Question <$> decodeQuestion pos1
    2 -> Answer <$> decodeAnswer pos1
    i -> Left $ Decode.BadType i

  where

  decodeQuestion :: Int -> Either Decode.Error CardWithID
  decodeQuestion pos0 = do
    { pos, val: msglen } <- Decode.unsignedVarint32 _xs_ pos0
    { val } <- tailRecM3 decodeQuestion_ (pos + msglen) { cardID: Nothing, card: { title: Nothing } } pos
    case val of
      { cardID: Just cardID, card: { title: Just title } } -> pure { cardID, card: { title, background: Nothing } }
      _ -> Left $ Decode.MissingFields "CardWithID"
      
  decodeQuestion_ :: Int -> CardWithID' -> Int -> Decode.Result' (Step { a :: Int, b :: CardWithID', c :: Int } { pos :: Int, val :: CardWithID' })
  decodeQuestion_ end acc pos1 | pos1 < end = do
    { pos: pos2, val: tag } <- Decode.unsignedVarint32 _xs_ pos1
    case tag `zshr` 3 of
      1 -> decodeFieldLoop end (Decode.string _xs_ pos2) \val -> acc { cardID = Just val }
      2 -> decodeFieldLoop end (Decode.string _xs_ pos2) \val -> acc { card = acc.card { title = Just val } }
      _ -> decodeFieldLoop end (Decode.skipType _xs_ pos2 $ tag .&. 7) \_ -> acc
  decodeQuestion_ _ acc pos1 = pure $ Done { pos: pos1, val: acc }

  decodeAnswer :: Int -> Either Decode.Error AnswerWithID
  decodeAnswer pos0 = do
    { pos, val: msglen } <- Decode.unsignedVarint32 _xs_ pos0
    { val } <- tailRecM3 decodeAnswer_ (pos + msglen) { cardID: Nothing, answer: Nothing } pos
    case val of
      { cardID: Just cardID, answer: Just answer } -> pure { cardID, answer }
      _ -> Left $ Decode.MissingFields "AnswerWithID"
      
  decodeAnswer_ :: Int -> AnswerWithID' -> Int -> Decode.Result' (Step { a :: Int, b :: AnswerWithID', c :: Int } { pos :: Int, val :: AnswerWithID' })
  decodeAnswer_ end acc pos1 | pos1 < end = do
    { pos: pos2, val: tag } <- Decode.unsignedVarint32 _xs_ pos1
    case tag `zshr` 3 of
      1 -> decodeFieldLoop end (Decode.string _xs_ pos2) \val -> acc { cardID = Just val }
      2 -> decodeFieldLoop end (Decode.string _xs_ pos2) \val -> acc { answer = Just val }
      _ -> decodeFieldLoop end (Decode.skipType _xs_ pos2 $ tag .&. 7) \_ -> acc
  decodeAnswer_ _ acc pos1 = pure $ Done { pos: pos1, val: acc }

decodeFieldLoop :: forall a b c. Int -> Decode.Result a -> (a -> b) -> Decode.Result' (Step { a :: Int, b :: b, c :: Int } { pos :: Int, val :: c })
decodeFieldLoop end res f = map (\{ pos, val } -> Loop { a: end, b: f val, c: pos }) res