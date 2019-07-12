{-
Welcome to a Spago project!
You can edit this file as you like.
-}

let mkPackage = ../mkPackage.dhall

in  { name =
        "purescript-concur-react-examples"
    , dependencies =
        [ "affjax"
        , "argonaut"
        , "concur-react"
        , "routing"
        ]
    , packages =
            ../packages.dhall
        //  { concur-react =
                { repo =
                    "../lib"
                , version =
                    ""
                , dependencies =
                    (../lib/spago.dhall).dependencies
                }
            }
    , sources =
        [ "src/**/*.purs", "test/**/*.purs" ]
    }
