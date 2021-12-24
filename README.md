# psel: Elisp backend for PureScript

WIP

## Motivation

For those who want to extend Emacs with PureScript instead of Elisp.
Nothing wrong with Elisp, but some prefers statically-typed languages over dynamically-typed languages.
However, since the generated elisp is non-human-friendly, it is not intended to register the generated elisp as an Elisp package to registries like MELPA.

## Installation

Currently, there is no release yet. To install `psel` command, you can build from source or use nix to install.

### Build from source

Requires `cabal-install` and `GHC` 8.10.7.

    git clone git@github.com:psel-org/psel.git
    cd psel
    cabal install

### Install using Nix(flake.nix)

Requires `nix` >= 2.4.

    nix profile install github:psel-org/psel

## Usage

Psel is intended to use through Spago. Set `backend = "psel"` in your `spago.dhall` file.
There is no package-set release yet. You can use WIP package-set `https://raw.githubusercontent.com/psel-org/package-sets/main/src/el-0.14.5-20211116/packages.dhall` for now.

```dhall
{ name = "foo"
, dependencies = [ "prelude" ]
, packages = https://raw.githubusercontent.com/psel-org/package-sets/main/src/el-0.14.5-20211116/packages.dhall
, backend = "psel"
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
```

`spago build` will output all .el files under `output.el` directory. To require from emacs, add this path to `load-path` variable.

## Module and Top-level bindings

`Data.Foo` module will be transpiled to `Data.Foo.el`.
Top-level binding `fooBar` in `Data.Foo` module will be transpiled to `(defvar Data.Foo.fooBar ...)`.

Though since we can't use `'` character for symbols in elisp, `'` inside binding name will be converted to `~`.
For example, `fooBar'` will be transpiled to `(defvar Data.Foo.fooBar~ ...)`.

## Type

Purescript | Elisp
-----------|------
Int | Integer
Double | Double
String | String
Array | Vector
Char | Integer(elisp doen't have char type)
Boolean | True -> t, False -> nil (elisp doesn't have boolean type)
Records | alist (e.g. `(('foo . 1) ('bar . "a"))`)
Unit | nil
Data types | Vector with constructor tag symbol in first slot and arguments in the remaining slots. (e.g. `Just 42` -> `['Just 42]`)

## TODO

* [x] Support FFI
* [x] Prelude fork
* [x] Make an github orgiznation
* [x] Make psel command spago friendly
* [x] Create package-set
* [ ] Support core libraries([WIP](https://github.com/psel-org/package-sets))
* [ ] Minimal example and add Usage document
* [ ] purescript-emacs
* [ ] Support contrib libraries
* [x] Write some tests
* [x] Add flake.nix
* [ ] Setup CI

## References

Learned how to write a CoreFn-type PureScript backend from [purenix](https://github.com/purenix-org/purenix) and [purerl](https://github.com/purerl/purerl).
