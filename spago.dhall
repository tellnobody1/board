{ name = "board"
, dependencies =
  [ "aff"
  , "affjax"
  , "arrays"
  , "console"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "integers"
  , "maybe"
  , "media-types"
  , "ordered-collections"
  , "prelude"
  , "psci-support"
  , "react"
  , "react-dom"
  , "strings"
  , "tuples"
  , "unsafe-coerce"
  , "web-dom"
  , "web-file"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
