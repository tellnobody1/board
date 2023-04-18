module Api where

import Control.Monad.Rec.Class (Step(Loop, Done), tailRecM3)
import Data.Either (Either(Left))
import Data.Int.Bits (shl, zshr, (.&.))
import Data.Maybe (Maybe(Just, Nothing))
import Prelude (map, bind, pure, (+), (<), ($), (<$>))
import Proto.Decode as Decode
import Proto.Encode as Encode
import Proto.Uint8Array (Uint8Array, length, concatAll)
import Types

type QuestionID' = Maybe String
type QuestionCard' = { title :: Maybe String }
type QuestionCardWithID' = { questionID :: QuestionID', questionCard :: QuestionCard' }

type Answer' = Maybe String
type AnswerWithID' = { questionID :: QuestionID', answer :: Answer' }
type Ask' = Maybe String

encode :: Api -> Uint8Array
encode (Question x) = concatAll [ Encode.unsignedVarint32 $ shl 1 3 + 2, encodeQuestion x ]
encode (Answer x) = concatAll [ Encode.unsignedVarint32 $ shl 2 3 + 2, encodeAnswer x ]
encode (Ask x) = concatAll [ Encode.unsignedVarint32 $ shl 3 3 + 2, encodeAsk x ]

encodeQuestion :: QuestionCardWithID -> Uint8Array
encodeQuestion msg = do
  let xs = concatAll
        [ Encode.unsignedVarint32 $ shl 1 3 + 2
        , Encode.string msg.questionID
        , Encode.unsignedVarint32 $ shl 2 3 + 2
        , Encode.string msg.questionCard.title
        ]
  concatAll [ Encode.unsignedVarint32 $ length xs, xs ]

encodeAnswer :: AnswerWithID -> Uint8Array
encodeAnswer msg = do
  let xs = concatAll
        [ Encode.unsignedVarint32 $ shl 1 3 + 2
        , Encode.string msg.questionID
        , Encode.unsignedVarint32 $ shl 2 3 + 2
        , Encode.string msg.answer
        ]
  concatAll [ Encode.unsignedVarint32 $ length xs, xs ]

encodeAsk :: QuestionID -> Uint8Array
encodeAsk msg = do
  let xs = concatAll
        [ Encode.unsignedVarint32 $ shl 1 3 + 2
        , Encode.string msg
        ]
  concatAll [ Encode.unsignedVarint32 $ length xs, xs ]

decode :: Uint8Array -> Either Decode.Error Api
decode _xs_ = do
  { pos: pos1, val: tag } <- Decode.unsignedVarint32 _xs_ 0
  case tag `zshr` 3 of
    1 -> Question <$> decodeQuestion pos1
    2 -> Answer <$> decodeAnswer pos1
    3 -> Ask <$> decodeAsk pos1
    i -> Left $ Decode.BadType i

  where

  decodeQuestion :: Int -> Either Decode.Error QuestionCardWithID
  decodeQuestion pos0 = do
    { pos, val: msglen } <- Decode.unsignedVarint32 _xs_ pos0
    { val } <- tailRecM3 decodeQuestion_ (pos + msglen) { questionID: Nothing, questionCard: { title: Nothing } } pos
    case val of
      { questionID: Just questionID, questionCard: { title: Just title } } -> pure { questionID, questionCard: { title, background: Nothing } }
      _ -> Left $ Decode.MissingFields "QuestionCardWithID"
      
  decodeQuestion_ :: Int -> QuestionCardWithID' -> Int -> Decode.Result' (Step { a :: Int, b :: QuestionCardWithID', c :: Int } { pos :: Int, val :: QuestionCardWithID' })
  decodeQuestion_ end acc pos1 | pos1 < end = do
    { pos: pos2, val: tag } <- Decode.unsignedVarint32 _xs_ pos1
    case tag `zshr` 3 of
      1 -> decodeFieldLoop end (Decode.string _xs_ pos2) \val -> acc { questionID = Just val }
      2 -> decodeFieldLoop end (Decode.string _xs_ pos2) \val -> acc { questionCard = acc.questionCard { title = Just val } }
      _ -> decodeFieldLoop end (Decode.skipType _xs_ pos2 $ tag .&. 7) \_ -> acc
  decodeQuestion_ _ acc pos1 = pure $ Done { pos: pos1, val: acc }

  decodeAnswer :: Int -> Either Decode.Error AnswerWithID
  decodeAnswer pos0 = do
    { pos, val: msglen } <- Decode.unsignedVarint32 _xs_ pos0
    { val } <- tailRecM3 decodeAnswer_ (pos + msglen) { questionID: Nothing, answer: Nothing } pos
    case val of
      { questionID: Just questionID, answer: Just answer } -> pure { questionID, answer }
      _ -> Left $ Decode.MissingFields "AnswerWithID"
      
  decodeAnswer_ :: Int -> AnswerWithID' -> Int -> Decode.Result' (Step { a :: Int, b :: AnswerWithID', c :: Int } { pos :: Int, val :: AnswerWithID' })
  decodeAnswer_ end acc pos1 | pos1 < end = do
    { pos: pos2, val: tag } <- Decode.unsignedVarint32 _xs_ pos1
    case tag `zshr` 3 of
      1 -> decodeFieldLoop end (Decode.string _xs_ pos2) \val -> acc { questionID = Just val }
      2 -> decodeFieldLoop end (Decode.string _xs_ pos2) \val -> acc { answer = Just val }
      _ -> decodeFieldLoop end (Decode.skipType _xs_ pos2 $ tag .&. 7) \_ -> acc
  decodeAnswer_ _ acc pos1 = pure $ Done { pos: pos1, val: acc }

  decodeAsk :: Int -> Either Decode.Error QuestionID
  decodeAsk pos0 = do
    { pos, val: msglen } <- Decode.unsignedVarint32 _xs_ pos0
    { val } <- tailRecM3 decodeAsk_ (pos + msglen) Nothing pos
    case val of
      Just questionID -> pure questionID
      _ -> Left $ Decode.MissingFields "QuestionID"
      
  decodeAsk_ :: Int -> Ask' -> Int -> Decode.Result' (Step { a :: Int, b :: Ask', c :: Int } { pos :: Int, val :: Ask' })
  decodeAsk_ end acc pos1 | pos1 < end = do
    { pos: pos2, val: tag } <- Decode.unsignedVarint32 _xs_ pos1
    case tag `zshr` 3 of
      1 -> decodeFieldLoop end (Decode.string _xs_ pos2) \val -> Just val
      _ -> decodeFieldLoop end (Decode.skipType _xs_ pos2 $ tag .&. 7) \_ -> acc
  decodeAsk_ _ acc pos1 = pure $ Done { pos: pos1, val: acc }

decodeFieldLoop :: forall a b c. Int -> Decode.Result a -> (a -> b) -> Decode.Result' (Step { a :: Int, b :: b, c :: Int } { pos :: Int, val :: c })
decodeFieldLoop end res f = map (\{ pos, val } -> Loop { a: end, b: f val, c: pos }) res
