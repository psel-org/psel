module Test.Main where

foreign import assertEqual :: forall a. String -> a -> a -> Boolean
foreign import data MainLike :: Type
foreign import mkMainLike :: forall a. ({} -> a) -> MainLike

main :: MainLike
main = mkMainLike main'

-- we don't have Effect nor Binding(do block) yet.
main' :: {} -> Array (Array Boolean)
main' _ =
  [ testCase {}
  ]

testCase :: {} -> Array Boolean
testCase _ =
  [ assertEqual "case int(1)" (caseInt 1) "a"
  , assertEqual "case int(2)" (caseInt 2) "b"
  , assertEqual "case int(3)" (caseInt 3) "c"
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
  , assertEqual "case multiple(1)" (caseMultiple 1 "a") "1a"
  , assertEqual "case multiple(2)" (caseMultiple 1 "c") "c"
  , assertEqual "case multiple(3)" (caseMultiple 2 "a") "2"
  , assertEqual "case multiple(4)" (caseMultiple 1 "b") "b"
  , assertEqual "case multiple(5)" (caseMultiple 1 "c") "c"
  ]

caseInt :: Int -> String
caseInt = case _ of
  1 -> "a"
  2 -> "b"
  _ -> "c"

caseBoolean :: Boolean -> String
caseBoolean = case _ of
  true -> "true"
  false -> "false"

caseRecord :: {a :: Int, b :: String} -> String
caseRecord = case _ of
  {a: 1} -> "a"
  {b: v, a: 2} -> v
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

caseMultiple :: Int -> String -> String
caseMultiple = case _,_ of
  1, "a" -> "1a"
  2, _   -> "2"
  _, "b" -> "b"
  _, _   -> "c"
