module Test.Main where

-- Pre-prelude world. We don't have == nor Effect yet.
-- Elisp特有の事情だが、レコードの比較は単純なequalでできないため別関数に分けている。
-- spagoからのmain関数起動は無引数の呼出しを想定しているが、
-- Effectモジュールが使えるまでFFIで関数を無引数関数で包む必要がある。
foreign import assertEqual :: forall a. String -> a -> a -> Boolean
foreign import assertEqualRecord :: forall r. String -> { | r }  -> { | r } -> Boolean
foreign import data MainLike :: Type
foreign import mkMainLike :: forall a. ({} -> a) -> MainLike

main :: MainLike
main = mkMainLike main'

-- we don't have Effect nor Binding(do block) yet.
main' :: {} -> Array (Array Boolean)
main' _ =
  [ testLet {}
  , testObject {}
  , testCase {}
  , testTypeClass {}
  ]

testLet :: {} -> Array Boolean
testLet _ =
  [ let a = 1
        b = [a, a]
    in assertEqual "let(1)" b [1,1]
  , let a [] = b [1]
        a [x] = b [x, x]
        a xs = xs
        b [] = a []
        b [x] = a [x]
        b [x, _] = a [x, x, x]
        b _ = []
    in assertEqual "letrec(1)" (b []) [1,1,1]
  ]

testObject :: {} -> Array Boolean
testObject _ =
  [ assertEqual "object access(1)" obj.a 1
  , assertEqual "object access(2)" obj.b 2
  , assertEqualRecord "object update(1)" (obj { a = 2 }) { a:2, b:2 }
  , assertEqualRecord "object update(2)" (obj { b = 1 }) { a:1, b:1 }
  ]
 where
  obj = { a: 1, b: 2 }

testCase :: {} -> Array Boolean
testCase _ =
  [ assertEqual "case int(1)" (caseInt 1) "a"
  , assertEqual "case int(2)" (caseInt 2) "b"
  , assertEqual "case int(3)" (caseInt 3) "c"
  , assertEqual "case number(1)" (caseNumber 1.1) "a"
  , assertEqual "case number(2)" (caseNumber 2.2) "b"
  , assertEqual "case number(3)" (caseNumber 1.11) "c"
  , assertEqual "case boolean(1)" (caseBoolean true) "true"
  , assertEqual "case boolean(2)" (caseBoolean false) "false"
  , assertEqual "case record(1)" (caseRecord { a: 1, b: "foo" }) "a"
  , assertEqual "case record(2)" (caseRecord { a: 2, b: "foo" }) "foo"
  , assertEqual "case record(3)" (caseRecord { a: 3, b: "foo" }) "c"
  , assertEqual "case array(1)" (caseArray []) "empty"
  , assertEqual "case array(2)" (caseArray ["a"]) "a"
  , assertEqual "case array(3)" (caseArray ["a","b","c"]) "b"
  , assertEqual "case array(4)" (caseArray ["a","b","b"]) "other"
  , assertEqual "case datatype(1)" (caseDataType Zero) "zero"
  , assertEqual "case datatype(2)" (caseDataType (One "a")) "a"
  , assertEqual "case datatype(3)" (caseDataType (Two "1" 1)) "1"
  , assertEqual "case datatype(4)" (caseDataType (Two "1" 2)) "other"
  , assertEqual "case newtype(1)" (caseNewType (NT "a")) "a!"
  , assertEqual "case newtype(2)" (caseNewType (NT "b")) "b"
  , assertEqual "case as(1)" (caseAs [[1,9],[3,4]]) [1,9]
  , assertEqual "case as(2)" (caseAs [[1,2],[3,9]]) [3,9]
  , assertEqual "case multiple(1)" (caseMultiple 1 "a") "1a"
  , assertEqual "case multiple(2)" (caseMultiple 1 "c") "c"
  , assertEqual "case multiple(3)" (caseMultiple 2 "a") "2"
  , assertEqual "case multiple(4)" (caseMultiple 1 "b") "b"
  , assertEqual "case multiple(5)" (caseMultiple 1 "c") "c"
  , assertEqual "case complex(1)" (caseComplex { a: [] }) []
  , assertEqual "case complex(2)" (caseComplex { a: [{a1: "a", a2: { a3: "b"}}] }) ["a"]
  , assertEqual "case complex(3)" (caseComplex { a: [{a1: "a", a2: { a3: "b"}}, {a1: "c", a2: { a3: "d"}}] }) ["a", "d"]
  ]

caseInt :: Int -> String
caseInt = case _ of
  1 -> "a"
  2 -> "b"
  _ -> "c"

caseNumber :: Number -> String
caseNumber = case _ of
  1.1 -> "a"
  2.2 -> "b"
  _ -> "c"

caseBoolean :: Boolean -> String
caseBoolean = case _ of
  true -> "true"
  false -> "false"

caseRecord :: {a :: Int, b :: String} -> String
caseRecord = case _ of
  { a: 1 } -> "a"
  { b: v, a: 2 } -> v
  _ -> "c"

caseArray :: Array String -> String
caseArray = case _ of
  [] -> "empty"
  [v] -> v
  ["a", v, "c"] -> v
  _ -> "other"

data DT
  = Zero
  | One String
  | Two String Int

caseDataType :: DT -> String
caseDataType = case _ of
  Zero -> "zero"
  One s -> s
  Two s 1 -> s
  Two _ _ -> "other"

newtype NT = NT String

caseNewType :: NT -> String
caseNewType = case _ of
  NT "a" -> "a!"
  NT s -> s

caseAs :: Array (Array Int) -> Array Int
caseAs = case _ of
  [v@[1, _], [3, 4]] -> v
  [[1, 2], v@[_, 9]] -> v
  _ -> []

caseMultiple :: Int -> String -> String
caseMultiple = case _,_ of
  1, "a" -> "1a"
  2, _   -> "2"
  _, "b" -> "b"
  _, _   -> "c"

caseComplex :: { a :: Array { a1 :: String,  a2 :: { a3 :: String }}} -> Array String
caseComplex = case _ of
  { a: [{ a1: "a"}] } -> ["a"]
  { a: [{ a1: v1, a2: _}, { a1: _, a2: { a3: v2}} ] } -> [v1, v2]
  _ -> []

testTypeClass :: {} -> Array Boolean
testTypeClass _ =
  [ assertEqual "type class(1)" (m1 1) "int"
  , assertEqual "type class(2)" (m2 1) 1
  , assertEqual "type class(3)" (m1 "b") "b"
  , assertEqual "type class(4)" (m2 "b") 0
  ]

class TC a where
  m1 :: a -> String
  m2 :: a -> Int

instance tcInt :: TC Int where
  m1 _ = "int"
  m2 a = a

instance tcString :: TC String where
  m1 s = s
  m2 _ = 0
