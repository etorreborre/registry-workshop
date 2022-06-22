module Test.Sessions.Show.Spec where

import qualified Data.Text as T
import Protolude
import Test.Sessions.Show.Solution
import Test.Tasty.Extensions

{-
  The purpose of this session is to display a registry
-}

test_display = test "we can display a registry" $ do
  let r1 =
        fun (\(n :: Int) -> (show n :: Text))
          +: val (10 :: Int)
          +: end -- aka Registry []
  display r1
    === T.unlines
      [ "VALUES",
        "10::Int",
        "",
        "FUNCTIONS",
        "Int -> Text"
      ]
