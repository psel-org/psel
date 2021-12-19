# psel: Elisp backend for PureScript

WIP

## Why

For those who want to extend Emacs with PureScript instead of Elisp.
Nothing wrong with Elisp, but some prefers statically-typed languages over dynamically-typed languages.
However, it is not intended to register the generated elisp as an Elisp package in registries like MELPA.

## Type Mapping

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
