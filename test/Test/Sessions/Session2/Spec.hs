module Test.Sessions.Session2.Spec where

import Data.Dynamic
import Protolude
-- import Session1.Data.Registry
import Test.Sessions.Session2.Solution
import Test.Tasty.Extensions

test_add = test "we can add elements to a registry in various ways" $ do
  -- the code should simply compile
  let r1 =
        (\(n :: Int) -> (show n :: Text))
          +: (10 :: Int)
          +: end

  let r2 = (10 :: Int) <+ r1
  let r3 = r1 <+ (10 :: Int)
  let r4 = r1 <+ r2
  let r5 = r2 <+ r1
  success
