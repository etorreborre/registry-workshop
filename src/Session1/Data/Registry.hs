module Session1.Data.Registry where

import Protolude

import Session1.Data.Function

-- | A Registry is just a list of dynamic functions
newtype Registry =
  Registry [Function]

infixr 5 +:

(+:) :: (Typeable a) => a -> Registry -> Registry
(+:) = panic "todo"

make :: forall a. Registry -> Either Text a
make = panic "todo"
