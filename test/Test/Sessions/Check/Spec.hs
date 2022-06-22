{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Test.Sessions.Check.Spec where

import Protolude
import Test.Sessions.Check.Solution
import Test.Tasty.Extensions

{-
  The purpose of this session is to make the API is more type safe
-}

test_add_typed = test "we can track the types of added elements" $ do
  -- the code should simply compile
  let _r1 :: Registry '[Int] '[Text, Int, Int] =
        fun (\(n :: Int) -> (show n :: Text))
          <+ val (100 :: Int)
          <+ val (10 :: Int)
  success

test_safe_add = test "we can only add an element if all its inputs can be built" $ do
  -- this code does not compile
  -- let _r1 =
  --       fun(\(n :: Double) -> (show n :: Text))
  --         <: val (100 :: Int)
  --         <: val (10 :: Int)
  -- this code compiles
  let _r1 =
         fun(\(n :: Double) -> (show n :: Text))
          <: val (100 :: Int)
          <: val (10 :: Double)
  success
