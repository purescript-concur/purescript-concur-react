{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "concur-react"
, dependencies =
  [ "aff"
  , "arrays"
  , "concur-core"
  , "console"
  , "effect"
  , "either"
  , "exceptions"
  , "maybe"
  , "prelude"
  , "react"
  , "react-dom"
  , "transformers"
  , "tuples"
  , "unsafe-coerce"
  , "web-dom"
  , "web-events"
  , "web-html"
  ]
, license = "MIT"
, repository = "https://github.com/purescript-concur/purescript-concur-react"
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
