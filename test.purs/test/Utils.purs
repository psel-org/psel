module Test.Utils where

foreign import assertEqual :: forall a. String -> a -> a -> Boolean
foreign import assertEqualRecord :: forall r. String -> { | r }  -> { | r } -> Boolean

-- for tests
foreign import eqInt :: Int -> Int -> Boolean
foreign import succInt :: Int -> Int
