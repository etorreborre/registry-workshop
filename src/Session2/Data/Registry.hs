module Session2.Data.Registry where

import Protolude

import Session2.Data.Function

-- | A Registry is just a list of dynamic functions
newtype Registry =
  Registry [Function]

end :: Registry
end = Registry mempty

-- | Append 2 registries together
(<+>) :: Registry -> Registry -> Registry
(<+>) = panic "todo <+>"

infixr 5 +:

(+:) :: (Typeable a) => a -> Registry -> Registry
(+:) = panic "todo"

infixr 4 <+

class Addable a b c | a b -> c where
  (<+) :: a -> b -> c
