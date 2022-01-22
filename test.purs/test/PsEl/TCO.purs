module Test.PsEl.TCO where

import Test.Utils

testTCO :: {} -> Array Boolean
testTCO _ =
  [ assertEqual "self recursion(1-1) 10" (selfRec1 0 10) 1
  , assertEqual "self recursion(1-2) 10000" (selfRec1 0 10000) 1
  , assertEqual "self recursion(2-1) 10" ((selfRec2 0 10) {}) 1
  , assertEqual "self recursion(2-2) 10000" ((selfRec2 0 10000) {}) 1
  , assertEqual "self recursion(3) 20" (selfRec3 0 20) 2
  , assertEqual "self recursion(4-1) 10" (selfRec4 0 10) 1
  -- , assertEqual "self recursion(4-2) 10000" (selfRec4 0 10000) 1
  , assertEqual "self recursion(5-1) 10" (selfRec5 0 10) 1
  -- , assertEqual "self recursion(5-2) 10000" (selfRec5 0 10000) 1
  , assertEqual "self recursion(6-1) 10" (selfRec6 0 10) 1
  -- , assertEqual "self recursion(6-2) 10000" (selfRec6 0 10000) 1
  , assertEqual "self recursion(7-1) 10" (selfRec7 0 10) 1
  -- , assertEqual "self recursion(7-2) 10000" (selfRec7 0 10000) 1
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

-- 再帰関数のローカル変数も再帰関数の場合。
-- top-downに再帰関数をloop化した場合でも正常に動作する必要あり。
-- これJS正しいのか？？？怪しいところ
selfRec5 :: Int -> Int -> Int
selfRec5 i to
  | eqInt i to = 1
  | eqInt i 10 = go i
  where
    go i
      | eqInt i 20 = go (succInt i)
      | true = selfRec5 i to
  | true = selfRec5 (succInt i) to

-- JS backend doesn't optimize this case, though it should be able.
-- Rare case.
selfRec6 :: Int -> Int -> Int
selfRec6 i to
  | eqInt i to = 1
  | true =
    let f = selfRec6 (succInt i)
    in f to

-- ローカルな自己再帰関数
selfRec7 :: Int -> Int -> Int
selfRec7 i to = go i to
  where
    go i to
      | eqInt i to = 1
      | true = go (succInt i) to

-- 型クラス使って辞書受け取るバージョンも
