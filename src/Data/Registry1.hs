module Data.Registry1 where

import Protolude

import Data.Function

-- | A Registry is just a list of dynamic functions
data Registry =
  Registry [Function]

infixr 5 +:

(+:) :: (Typeable a) => a -> Registry -> Registry
(+:) = panic "todo"

make :: forall a. Registry -> Either Text a
make = panic "todo"
