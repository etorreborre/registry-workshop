{-# LANGUAGE PolyKinds #-}

module Test.Sessions.Add.Solution where

import Data.Dynamic
import GHC.Exts
import Protolude hiding (TypeRep, typeOf, typeRep)
import Type.Reflection

newtype Registry
  = Registry [Function]

end :: Registry
end = Registry mempty

data Function
  = Function Dynamic FunctionType

data FunctionType = FunctionType {inputs :: [Text], output :: Text}
  deriving (Eq, Show)

data Value
  = Value Dynamic Text

-- | Create a dynamic function, keeping both the function as Dynamic and its type
createFunction :: forall a. (Typeable a) => a -> Function
createFunction a = Function (toDyn a) (getFunctionType (typeRep @a))

-- | Extract the input types and output type of a function
getFunctionType :: forall (r1 :: RuntimeRep) (arg :: TYPE r1). TypeRep arg -> FunctionType
getFunctionType a =
  case a of
    Fun t1 t2 -> do
      let in1 = show t1
      let FunctionType ins out = getFunctionType t2
      FunctionType (in1 : ins) out
    _other ->
      FunctionType [] (show (SomeTypeRep a))

-- | Apply a Dynamic parameter to a Dynamic function
applyParameter :: Dynamic -> Dynamic -> Either Text Dynamic
applyParameter f i = maybe (Left $ "failed to apply " <> show i <> " to : " <> show f) Right (dynApply f i)

infixr 5 +:

(+:) :: forall a. (Typeable a) => a -> Registry -> Registry
(+:) a (Registry as) = Registry (createFunction a : as)

-- | Append 2 registries together
(<+>) :: Registry -> Registry -> Registry
(<+>) (Registry fs1) (Registry fs2) = Registry $ fs1 <> fs2

infixr 4 <+

class Addable a b c | a b -> c where
  (<+) :: a -> b -> c

instance {-# OVERLAPPING #-} Addable Registry Registry Registry where
  (<+) = (<+>)

instance {-# OVERLAPPING #-} (Typeable a) => Addable a Registry Registry where
  (<+) a r = a +: r

instance {-# OVERLAPPING #-} (Typeable a) => Addable Registry a Registry where
  (<+) r a = a +: r

instance {-# OVERLAPPABLE #-} (Typeable a, Typeable b) => Addable a b Registry where
  (<+) a b = a +: b +: end
