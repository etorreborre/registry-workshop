{-# LANGUAGE PolyKinds #-}

module Session2.Data.Function where

import Data.Dynamic
import GHC.Exts
import Protolude hiding (TypeRep)
import Type.Reflection

-- | Representation of a Haskell function and its signature
data Function
  = Function Dynamic FunctionType

data FunctionType = FunctionType {inputs :: [Text], output :: Text}
  deriving (Eq, Show)

-- | Create a dynamic function, keeping both the function as Dynamic and its type
createFunction :: (Typeable a) => a -> Function
createFunction = panic "todo - createFunction"

-- | Apply a Dynamic parameter to a Dynamic function
applyParameter :: Dynamic -> Dynamic -> Either Text Dynamic
applyParameter = panic "todo - applyParameter"

-- | Apply a Dynamic function to a list of Dynamic values
applyFunction :: Dynamic -> [Dynamic] -> Either Text Dynamic
applyFunction = panic "todo - applyFunction"

-- | Extract the input types and output type of a function
--   (even if it is reduced to just an output value with no input parameters)
--   hint: use the typeRep function to access the type of a value
getFunctionType :: forall a. (Typeable a) => a -> FunctionType
getFunctionType = panic "todo - getFunctionType"

-- | Extract the input types and output type of a function
--   hint: pattern match on the Fun constructor
getFunctionType' :: forall (r1 :: RuntimeRep) (arg :: TYPE r1). TypeRep arg -> FunctionType
getFunctionType' = panic "todo - getFunctionType'"
