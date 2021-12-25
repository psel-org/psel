module Test.PsEl.TCO where

import Test.Utils

testTCO :: {} -> Array Boolean
testTCO _ =
  [ assertEqual "self recursion 10" (selfRec1 0 10) 1
  -- , assertEqual "self recursion 100" (selfRec1 0 100) 1
  -- , assertEqual "self recursion 1000" (selfRec1 0 1000) 1
  , assertEqual "self recursion(2) 10" ((selfRec2 0 10) {}) 1
  , assertEqual "self recursion(3) 20" (selfRec3 0 20) 2
  , assertEqual "self recursion(4) 10" (selfRec4 0 10) 1
  ]

selfRec1 :: Int -> Int -> Int
selfRec1 i to
  | eqInt i to = 1
  | true = selfRec1 (succInt i) to

selfRec2 :: Int -> Int -> {} -> Int
selfRec2 i to
  | eqInt i to = \_ -> 1
  | true = selfRec2 (succInt i) to

-- 末尾呼出しと非末尾呼出しが混在している場合、結構ややこい
-- JS backend doesn't apply TCO for this function.
-- 多分非末尾呼出しが混在しているからかな。末尾呼出しの方が取れほど呼ばれるかは不明だからか。
selfRec3 :: Int -> Int -> Int
selfRec3 i to
  | eqInt i to = 1
  | eqInt i 10 = succInt (selfRec3 (succInt i) to)   -- non-recursive call
  | true = selfRec3 (succInt i) to                   -- recursive call

-- lambda
selfRec4 :: Int -> Int -> Int
selfRec4 i to = go i
  where
    go i
      | eqInt i to = 1
      | true = selfRec4 (succInt i) to

-- 型クラス使って辞書受け取るバージョンも
