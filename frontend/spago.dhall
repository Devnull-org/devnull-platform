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
  , "halogen-formless"
  , "pathy"
  , "precise-datetime"
  , "psci-support"
  , "refs"
  , "routing"
  , "routing-duplex"
  , "simple-json"
  , "slug"
  , "strings"
  , "tolerant-argonaut"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
