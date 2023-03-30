module Lib.Ajax
  ( getEff
  , getBlobEff
  ) where

import Prelude

import Affjax.Web as Affjax
import Affjax.RequestHeader (RequestHeader)
import Affjax.ResponseFormat as ResponseFormat
import Affjax.StatusCode (StatusCode(StatusCode))
import Data.Either (Either(Left, Right))
import Effect (Effect)
import Effect.Aff (Aff, runAff_)
import Effect.Exception (Error)
import Effect.Class.Console (errorShow)
import Web.File.Blob (Blob)

get :: String -> Aff (Either String String)
get url = do
  resp <- Affjax.get ResponseFormat.string url
  pure $ case resp of
    Left err -> Left $ "Request failed: " <> (Affjax.printError err)
    Right { status: StatusCode  200, body: d } -> Right d
    Right { status: StatusCode code, body: b } -> Left $ "Request failed: " <> (show code) <> ", b:" <> b

getEff :: forall a. String -> (String -> Effect a) -> Effect Unit
getEff url success = runAff_ f (get url)
  where
    f :: Either Error (Either String String) -> Effect Unit
    f (Left e) = void $ errorShow e
    f (Right (Left e)) = void $ errorShow e
    f (Right (Right a)) = void $ success a

getBlob :: String -> Array RequestHeader -> Aff (Either String Blob)
getBlob url headers = do
  resp <- Affjax.request (Affjax.defaultRequest { url = url, headers = headers, responseFormat = ResponseFormat.blob })
  pure $ case resp of
    Left err -> Left $ "Request failed: " <> (Affjax.printError err)
    Right { status: StatusCode  200, body: d } -> Right d
    Right { status: StatusCode code } -> Left $ "Request failed: " <> (show code)

getBlobEff :: forall a. String -> Array RequestHeader -> (Blob -> Effect a) -> Effect Unit
getBlobEff url headers success = runAff_ f (getBlob url headers)
  where
    f :: Either Error (Either String Blob) -> Effect Unit
    f (Left e) = void $ errorShow e
    f (Right (Left e)) = void $ errorShow e
    f (Right (Right a)) = void $ success a
