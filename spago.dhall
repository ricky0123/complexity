{-
Welcome to a Spago project!
You can edit this file as you like.

Need help? See the following resources:
- Spago documentation: https://github.com/purescript/spago
- Dhall language tour: https://docs.dhall-lang.org/tutorials/Language-Tour.html

When creating a new Spago project, you can use
`spago init --no-comments` or `spago init -C`
to generate this file without the comments in this block.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "arrays"
  , "console"
  , "datetime"
  , "effect"
  , "either"
  , "enums"
  , "foldable-traversable"
  , "foreign"
  , "halogen"
  , "halogen-formless"
  , "halogen-subscriptions"
  , "integers"
  , "js-promise-aff"
  , "lists"
  , "maybe"
  , "newtype"
  , "now"
  , "optparse"
  , "partial"
  , "prelude"
  , "random"
  , "refs"
  , "routing"
  , "spec"
  , "spec-node"
  , "strings"
  , "tailrec"
  , "transformers"
  , "tuples"
  , "unfoldable"
  , "web-dom"
  , "web-events"
  , "web-html"
  , "web-uievents"
  , "web-workers"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
