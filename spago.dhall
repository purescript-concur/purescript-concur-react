{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "concur-react"
, dependencies =
    [ "react"
    , "arrays"
    , "aff"
    , "avar"
    , "tailrec"
    , "free"
    , "nonempty"
    , "foldable-traversable"
    , "react-dom"
    , "web-dom"
    , "web-html"
    , "console"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
