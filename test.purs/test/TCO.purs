module Test.TCO where

import Test.Utils

foreign import eqInt :: Int -> Int -> Boolean
foreign import succInt :: Int -> Int

testTCO :: {} -> Array Boolean
testTCO _ =
  [ assertEqual "self recursion 10" (selfRec1 0 10) "done"
  -- , assertEqual "self recursion 100" (selfRec1 0 100) "done"
  -- , assertEqual "self recursion 1000" (selfRec1 0 1000) "done"
  , assertEqual "self recursion(2) 10" ((selfRec2 0 10) {}) "done"
  , assertEqual "self recursion(3) 20" (selfRec3 20 0) 2
  ]

selfRec1 :: Int -> Int -> String
selfRec1 i to
  | eqInt i to = "done"
  | true = selfRec1 (succInt i) to

selfRec2 :: Int -> Int -> {} -> String
selfRec2 i to
  | eqInt i to = \_ -> "done"
  | true = selfRec2 (succInt i) to

-- 末尾呼出しと非末尾呼出しが混在している場合、結構ややこい
-- selfRec3 to = g としてしまうと f の計算で無限ループになってしまう(多分JSも)。
selfRec3 :: Int -> Int -> Int
selfRec3 to i = g i
  where
    f = selfRec3 to
    g i
      | eqInt i to = 1
      | eqInt i 10 = succInt (f (succInt i))   -- non-recursive call
      | true = f (succInt i)                   -- recursive call

-- 型クラス使って辞書受け取るバージョンも
