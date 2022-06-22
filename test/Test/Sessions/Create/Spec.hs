module Test.Sessions.Create.Spec where

import Data.Dynamic
import Protolude
-- import Session1.Data.Registry
import Test.Sessions.Create.Solution
import Test.Tasty.Extensions

{-
  The purpose of this session is to implement the basic data structure for
  storing dynamic values and applying dynamic functions to dynamic values
-}

-- * SUPPORT FOR FUNCTIONS REFLECTION

test_function_type = test "we can get the type of a function" $ do
  let Function _ ft = createFunction (\(n :: Int) (d :: Double) -> show n <> "_" <> show d :: Text)
  ft === FunctionType ["Int", "Double"] "Text"

test_apply_function_parameters = test "we can apply dynamic parameters to a dynamic function" $ do
  let result = applyFunction (toDyn (\(n :: Int) (d :: Double) -> show n <> "_" <> show d :: Text)) [toDyn (1 :: Int), toDyn (2 :: Double)]
  case result of
    Left e -> annotateShow e >> failure
    Right r -> fromDynamic r === Just ("1_2.0" :: Text)

-- * REGISTRY CREATION

test_create_registry = test "create a registry with values and functions" $ do
  let _r =
        (\(n :: Int) -> show n :: Text)
          +: (10 :: Int)
          +: Registry []
  success

-- * MAKING VALUES

test_make_simple_value = test "we retrieve an existing value" $ do
  let r =
        (\(n :: Int) -> (show n :: Text))
          +: (10 :: Int)
          +: Registry []

  make @Int r === Right 10

test_make_value = test "we can make a value by doing function application" $ do
  let r =
        (\(n :: Int) -> (show n :: Text))
          +: (10 :: Int)
          +: Registry []

  make @Text r === Right "10"

test_make_value_deep = test "we can make a value by repeated function calls" $ do
  let r =
        Person
          +: Age
          +: Name
          +: (\(n :: Int) -> (show n :: Text))
          +: (10 :: Int)
          +: Registry []

  make @Age r === Right (Age 10)
  make @Name r === Right (Name "10")
  make @Person r === Right (Person (Name "10") (Age 10))

test_override_value = test "we can override a value for a given type" $ do
  let r =
        (12 :: Int)
          +: (11 :: Int)
          +: Person
          +: Age
          +: Name
          +: (\(n :: Int) -> (show n :: Text))
          +: (10 :: Int)
          +: Registry []

  make @Age r === Right (Age 12)
  make @Name r === Right (Name "12")
  make @Person r === Right (Person (Name "12") (Age 12))

-- * HELPERS

newtype Name = Name Text
  deriving (Eq, Show)

newtype Age = Age Int
  deriving (Eq, Show)

data Person = Person Name Age
  deriving (Eq, Show)
