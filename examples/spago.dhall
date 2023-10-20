{ name = "halogen-erlesen-examples"
, dependencies =
  [ "aff"
  , "arrays"
  , "console"
  , "effect"
  , "halogen"
  , "halogen-erlesen"
  , "js-date"
  , "maybe"
  , "prelude"
  , "stringutils"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
