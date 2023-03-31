module Lib.React where

import Prelude
import Effect (Effect)
import React.DOM.Props as R
import Unsafe.Coerce (unsafeCoerce)

cn :: String -> R.Props
cn = R.className

onChange :: (String -> Effect Unit) -> R.Props
onChange f = R.onChange \e -> f $ (unsafeCoerce e).target.value
