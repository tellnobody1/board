module Lib.React where

import Effect (Effect)
import Prelude
import React (ReactElement)
import React.DOM.Props as R
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Element (Element)

cn :: String -> R.Props
cn = R.className

onChange :: (String -> Effect Unit) -> R.Props
onChange f = R.onChange \e -> f $ (unsafeCoerce e).target.value

foreign import data Root :: Type

foreign import createRoot :: Element -> Effect Root

foreign import render :: Root -> ReactElement -> Effect Unit
