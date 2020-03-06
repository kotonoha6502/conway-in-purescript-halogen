{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
    [ "console"
    , "css"
    , "effect"
    , "halogen"
    , "halogen-bootstrap"
    , "halogen-css"
    , "psci-support"
    , "sized-vectors"
    , "transformers"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
