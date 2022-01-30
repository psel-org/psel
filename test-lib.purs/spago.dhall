{ name = "test-lib"
, dependencies =
  [ "prelude", "effect", "console", "assert", "refs", "lists", "functions" ]
, packages =
    https://raw.githubusercontent.com/psel-org/package-sets/main/src/el-0.14.5-20211116/packages.dhall
, backend = "psel"
, sources = [ "test/**/*.purs" ]
}
