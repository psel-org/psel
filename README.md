# psel: Elisp backend for PureScript

WIP

## Motivation

Elisp is powerful and flexible, but when it comes to complex logic code, some might prefer strongly typed language.
Since it is possible to call PS functions from elisp, it is possible to write partially in PS.
Also, you can reuse PureScript libraries as long as they do not depend on JS/browser specific libraries.

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
{ name = "your-project-name"
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

## Types with special treatment

### Uncurried types

Not done yet.

`Data.Function.Uncurried(funcitons package)` and `Effect.Uncurried(effects pacakge)`

## Optimization

### TCO(Tail-Call Optimization)

Currently, TCO is only applied to certain forms of self-recursion functions.
TCO will convert these self-resursive calls to `while` s-exp expression.

### MagicDo

Not done yet.

reference
https://github.com/purescript/purescript-effect#the-effect-type-is-magic

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
