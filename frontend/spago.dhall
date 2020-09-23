{ name = "devnull"
, dependencies =
  [ "aff"
  , "aff-bus"
  , "aff-promise"
  , "affjax"
  , "argonaut"
  , "console"
  , "debug"
  , "dotenv"
  , "effect"
  , "halogen"
  , "pathy"
  , "precise-datetime"
  , "psci-support"
  , "refs"
  , "routing-duplex"
  , "slug"
  , "strings"
  , "tolerant-argonaut"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
