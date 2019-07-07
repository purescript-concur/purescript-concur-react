{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "concur-react"
, dependencies =
    [ "aff"
    , "arrays"
    , "avar"
    , "console"
    , "foldable-traversable"
    , "free"
    , "nonempty"
    , "react"
    , "react-dom"
    , "tailrec"
    , "web-dom"
    , "web-html"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
