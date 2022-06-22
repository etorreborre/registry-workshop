module Test.Sessions.Add.Spec where

import Protolude
import Test.Sessions.Add.Solution
import Test.Tasty.Extensions

{-
  The purpose of this session is to implement a versatile "add"
  operator to improve the API
-}

test_better_add = test "we can add elements to a registry in various ways" $ do
  -- the code should simply compile
  let r1 =
        (\(n :: Int) -> (show n :: Text))
          +: (10 :: Int)
          +: end
  let r2 = (10 :: Int) <+ r1
  let r3 = r2 <+ (10 :: Int)
  let r4 = (10 :: Int) <+ ("10" :: Text)
  let _r5 = r3 <+ r4
  success
