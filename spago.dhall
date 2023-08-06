{ name = "halogen-erlesen"
, dependencies = 
  [ "prelude"
  , "arrays"
  , "maybe" 
  , "arrays"
  , "console"
  , "effect"
  , "foldable-traversable"
  , "halogen"
  , "maybe"
  , "prelude"
  , "transformers"
  , "typelevel-prelude"
  , "web-dom"
  , "web-events"
  , "web-html"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
