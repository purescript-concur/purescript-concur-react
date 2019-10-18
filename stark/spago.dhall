{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "concur-stark"
, dependencies =
    [ "aff"
    , "arrays"
    , "avar"
    , "console"
    , "concur-core"
    , "foldable-traversable"
    , "free"
    , "nonempty"
    , "profunctor-lenses"
    , "tailrec"
    , "web-dom"
    , "web-html"
    ]
, packages =
    ../packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
