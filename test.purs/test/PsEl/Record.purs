module Test.PsEl.Record where

import Test.Utils

testRecord :: {} -> Array Boolean
testRecord _ =
  [ assertEqual "record access(1)" rec1.a 1
  , assertEqual "record access(2)" rec1.a' 2
  , assertEqual "record access(3)" rec1." a+-*/{}()'\"\\ " 3
  , assertEqual "record access(4)" rec1."123" 4
  , assertEqualRecord "record update(1)" (rec2 { a = 2 }) { a:2, b:2 }
  , assertEqualRecord "record update(2)" (rec2 { b = 1 }) { a:1, b:1 }
  ]
 where
  -- レコード任意の文字列をフォールド名として持てる
  rec1 =
    { a: 1
    , a': 2
    , " a+-*/{}()'\"\\ ": 3
    , "123": 4
    }

  rec2 = { a: 1, b: 2 }
