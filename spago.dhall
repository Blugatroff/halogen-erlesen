{ name = "halogen-erlesen"
, dependencies =
  [ "arrays"
  , "foldable-traversable"
  , "halogen"
  , "maybe"
  , "prelude"
  , "stringutils"
  , "transformers"
  , "typelevel-prelude"
  , "web-dom"
  , "web-events"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
