{ name = "board"
, dependencies =
  [ "aff"
  , "affjax"
  , "affjax-web"
  , "arrays"
  , "console"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "maybe"
  , "media-types"
  , "ordered-collections"
  , "prelude"
  , "react"
  , "react-dom"
  , "simple-json"
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
