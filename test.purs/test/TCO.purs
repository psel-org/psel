module Test.TCO where

import Test.Utils

foreign import eqInt :: Int -> Int -> Boolean
foreign import succInt :: Int -> Int

testTCO :: {} -> Array Boolean
testTCO _ =
  [ assertEqual "self recursion 100" (selfRec 0 100) "done"
  , assertEqual "self recursion 1000" (selfRec 0 1000) "done"
  , assertEqual "self recursion 10000" (selfRec 0 10000) "done"
  ]

selfRec :: Int -> Int -> String
selfRec i to
  | eqInt i to = "done"
  | true = selfRec (succInt i) to
