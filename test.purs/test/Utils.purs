module Test.Utils where

foreign import assertEqual :: forall a. String -> a -> a -> Boolean
foreign import assertEqualRecord :: forall r. String -> { | r }  -> { | r } -> Boolean
