module Lib.Ninjas where

import Affjax.RequestHeader (RequestHeader(..))
import Control.Bind (composeKleisli)
import Data.MediaType (MediaType(..))
import Effect (Effect)
import Lib.Affjax (getBlobEff)
import Prelude
import Web.File.Url (createObjectURL)

randomImage :: Int -> (String -> Effect Unit) -> Effect Unit
randomImage i f = getBlobEff (imagePath <> show i) imageHeaders $ composeKleisli createObjectURL f

imagePath :: String
imagePath = "https://api.api-ninjas.com/v1/randomimage?category=nature&width=400&height=300"

imageHeaders :: Array RequestHeader
imageHeaders =
  [ Accept $ MediaType "image/jpg"
  , RequestHeader "X-Api-Key" "0YJRpHZcyfY185HL1U2cPA==kJYRAKy5BmMcEWHD"
  ]
