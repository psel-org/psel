{-# LANGUAGE OverloadedStrings #-}

module PsEl.SExpConstructor where

import PsEl.SExp
import RIO

nil :: SExp
nil = symbol "nil"

t :: SExp
t = symbol "t"

require :: FeatureName -> SExp
require (FeatureName name) =
    funcallNative "require" [quotedSymbol name]

provide :: FeatureName -> SExp
provide (FeatureName name) =
    funcallNative "provide" [quotedSymbol name]

funcall0 :: SExp -> SExp
funcall0 f =
    funcallNative "funcall" [f]
