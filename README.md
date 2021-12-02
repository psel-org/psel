# psel: Elisp backend for PureScript

## Why

For those who want to extend Emacs with PureScript instead of Elisp.
Nothing wrong with Elisp, but some prefers statically-typed languages than dinamically-typed languages.
However, it is not intended to register the generated elisp as an Elisp package in registries like MELPA.

## Implementation

### Type Mapping

Purescript | Elisp
-----------|------
Int | Integer
Double | Double
String | String
Array | Vector
Char | Integer(elisp doen't have char type)
Boolean | True -> t, False -> nil (elisp doesn't have boolean type)
Records | alist (e.g. `(('foo . 1) ('bar . "a")`)
Unit | nil (not implemented yet)
Data types | Vector with constructor tag symbol in first slot and arguments in the remaining slots. (e.g. `Just 42` -> `['Just 42]`)

## TODO

* [ ] Support FFI
* [ ] Prelude fork
* [ ] Minimal example and add Usage document
* [ ] Support Aff
* [ ] Write some tests

## References

Learned how to write a CoreFn-type PureScript backend from [purenix](https://github.com/purenix-org/purenix).
