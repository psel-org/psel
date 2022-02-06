module Test.Main where

import Prelude
import Effect
import Effect.Ref as Ref
import Effect.Console (log)
import Test.Assert (assertEqual)
import Data.List
import Data.Tuple as Tuple
import Data.Function.Uncurried

main :: Effect Unit
main = do
  testApply
  testEffect
  testList
  testTuple
  testFunctionUncurried

testApply :: Effect Unit
testApply = do
  log "apply(1)"
  assertEqual
    { actual: (+) 1 $ 1
    , expected: 2
    }
  log "apply(2)"
  assertEqual
    { actual: (+) 1 $ (+) 1 $ 1
    , expected: 3
    }
  log "applyFlipped(1)"
  assertEqual
    { actual: 1 # (+) 1
    , expected: 2
    }
  log "applyFlipped(2)"
  assertEqual
    { actual: 1 # (+) 1 # (+) 1
    , expected: 3
    }

testEffect :: Effect Unit
testEffect = do
  log "effect(1)"
  v <- testEffect1
  assertEqual { actual: v, expected: 4 }

-- disgarding effects
testEffect1 :: Effect Int
testEffect1 = do
  ref <- Ref.new 1
  succVoid ref
  _ <- succ ref
  succVoid ref
  val <- Ref.read ref
  pure val
 where
  succ ref = Ref.modify (_ + 1) ref
  succVoid ref = void $ Ref.modify (_ + 1) ref

foreign import nativeList :: List Int

testList :: Effect Unit
testList = do
  let l = (1..3)
  log "list(1)"
  assertEqual { actual: nativeList, expected: l }
  log "list(2)"
  assertEqual { actual: f nativeList, expected: 6 }
  assertEqual { actual: f l, expected: 6 }
 where
  f (Cons a (Cons b (Cons c Nil))) = a + b + c
  f _ = 0

testTuple :: Effect Unit
testTuple = do
  log "tuple(1)"
  assertEqual { actual: Tuple.fst a, expected: 1 }
  log "tuple(2)"
  assertEqual { actual: Tuple.snd a, expected: "a" }
 where
  a = Tuple.Tuple 1 "a"

foreign import add2 :: Fn2 Int Int Int

add2Wrapper :: Int -> Int -> Int
add2Wrapper a b = runFn2 add2 a b

testFunctionUncurried :: Effect Unit
testFunctionUncurried = do
  log "function uncurried(1)"
  assertEqual { actual: runFn2 add2 1 2, expected: 3 }
  log "function uncurried(2)"
  let t = runFn2 add2 1
  assertEqual { actual: t 3, expected: 4 }
  assertEqual { actual: t 4, expected: 5 }
  log "function uncurried(3)"
  assertEqual { actual: add2Wrapper 1 2, expected: 3 }
