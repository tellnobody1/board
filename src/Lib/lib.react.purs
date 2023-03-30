module Lib.React where

import Effect (Effect)
import Prelude hiding (div)
import React.DOM.Props (Props, className, onChange)
import Unsafe.Coerce (unsafeCoerce)

cn :: String -> Props
cn = className

onChangeValue :: (String -> Effect Unit) -> Props
onChangeValue f = onChange \e -> f $ (unsafeCoerce e).target.value
