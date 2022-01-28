module Test.Main where

import Prelude
import Effect
import Effect.Ref as Ref
import Effect.Console (log)
import Test.Assert (assertEqual)

main :: Effect Unit
main = do
  testApply
  testEffect

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
