{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Test.Sessions.Check.Solution where

import Data.Dynamic
import GHC.Exts
import GHC.TypeLits hiding (Text)
import qualified GHC.TypeLits as TL
import Protolude hiding (TypeError, TypeRep, typeOf, typeRep)
import Type.Reflection

data Registry ins out
  = Registry [Value] [Function]

end :: Registry '[] '[]
end = Registry mempty mempty

data Typed a
  = TypedValue Value
  | TypedFunction Function

fun :: Typeable a => a -> Typed a
fun = TypedFunction . createFunction

val :: (Typeable a, Show a) => a -> Typed a
val = TypedValue . createValue

data Function
  = Function Dynamic FunctionType

data FunctionType = FunctionType {inputs :: [Text], output :: Text}
  deriving (Eq, Show)

data Value = Value
  { dynValue :: Dynamic,
    valueType :: Text,
    valueShow :: Maybe Text
  }

-- | Create a dynamic value
createValue :: forall a. (Typeable a, Show a) => a -> Value
createValue a = do
  let FunctionType _ out = getFunctionType (typeRep @a)
  Value (toDyn a) out (Just $ show a)

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

infixr 5 +:

-- | Prepend a value to a registry
(+:) :: (Typeable a) => Typed a -> Registry ins out -> Registry (Inputs a :++ ins) (Output a ': out)
(+:) (TypedValue v) (Registry vs fs) = Registry (v : vs) fs
(+:) (TypedFunction f) (Registry vs fs) = Registry vs (f : fs)

-- | Append a value to a registry
append :: (Typeable a) => Registry ins out -> Typed a -> Registry (ins :++ Inputs a) (out :++ '[Output a])
append (Registry vs fs) (TypedValue v) = Registry (vs <> [v]) fs
append (Registry vs fs) (TypedFunction f) = Registry vs (fs <> [f])

-- | Append 2 values together
add :: (Typeable a, Typeable b, ins ~ Inputs a :++ Inputs b, out ~ [Output a, Output b]) => Typed a -> Typed b -> Registry ins out
add (TypedValue v1) (TypedValue v2) = Registry [v1, v2] []
add (TypedValue v1) (TypedFunction f2) = Registry [v1] [f2]
add (TypedFunction f1) (TypedFunction f2) = Registry [] [f1, f2]
add (TypedFunction f1) (TypedValue v2) = Registry [v2] [f1]

-- | Compute the list of input types for a function
type family Inputs f :: [Type] where
  Inputs (i -> o) = i ': Inputs o
  Inputs x = '[]

-- | Compute the output type for a function
type family Output f :: Type where
  Output (i -> o) = Output o
  Output x = x

-- | Append 2 registries together
(<+>) :: Registry is1 os1 -> Registry is2 os2 -> Registry (is1 :++ is2) (os1 :++ os2)
(<+>) (Registry vs1 fs1) (Registry vs2 fs2) = Registry (vs1 <> vs2) (fs1 <> fs2)

-- | Append 2 lists of types
type family (:++) (x :: [k]) (y :: [k]) :: [k] where
  '[] :++ xs = xs
  (x ': xs) :++ ys = x ': (xs :++ ys)

-- | Create registries without checking if values can be created
infixr 4 <+

class Addable a b c | a b -> c where
  (<+) :: a -> b -> c

instance (is3 ~ (is1 :++ is2), os3 ~ (os1 :++ os2)) => Addable (Registry is1 os1) (Registry is2 os2) (Registry is3 os3) where
  (<+) = (<+>)

instance (Typeable a, is2 ~ (Inputs a :++ is1), os2 ~ (Output a : os1)) => Addable (Typed a) (Registry is1 os1) (Registry is2 os2) where
  (<+) = (+:)

instance (Typeable a, is2 ~ is1 :++ Inputs a, os2 ~ os1 :++ '[Output a]) => Addable (Registry is1 os1) (Typed a) (Registry is2 os2) where
  (<+) = append

instance (Typeable a, Typeable b, ins ~ Inputs a :++ Inputs b, out ~ [Output a, Output b]) => Addable (Typed a) (Typed b) (Registry ins out) where
  (<+) = add

-- | Create registries without checking if values can be created
infixr 4 <:

class AddableChecked a b c | a b -> c where
  (<:) :: a -> b -> c

instance (is3 ~ (is1 :++ is2), os3 ~ (os1 :++ os2)) => AddableChecked (Registry is1 os1) (Registry is2 os2) (Registry is3 os3) where
  (<:) = (<+>)

instance (Typeable a, IsSubset (Inputs a) os2 a, is2 ~ (Inputs a :++ is1), os2 ~ (Output a : os1)) => AddableChecked (Typed a) (Registry is1 os1) (Registry is2 os2) where
  (<:) = (+:)

instance (Typeable a, is2 ~ is1 :++ Inputs a, os2 ~ os1 :++ '[Output a]) => AddableChecked (Registry is1 os1) (Typed a) (Registry is2 os2) where
  (<:) = append

instance (Typeable a, Typeable b, ins ~ Inputs a :++ Inputs b, out ~ [Output a, Output b]) => AddableChecked (Typed a) (Typed b) (Registry ins out) where
  (<:) = add

-- | Instances of this class show that the ins types are included in the list of out types
--   The search is performed for a given type a which is used to render error messages
class IsSubset (ins :: [Type]) (out :: [Type]) (target :: Type)

-- | Whatever the type a, an empty list is always included in a list of types
instance IsSubset '[] out a

-- | The list of elements: a + els is a subset of out if els is a subset of out and
--   a is also included in the set out. The search for a in out is done via a
--   type family in order to be able to display an error message if it can't be found
instance (CanMake a out t, IsSubset els out t) => IsSubset (a ': els) out t

-- | Compute if a constructor can be added to a registry
type family CanMake (a :: Type) (els :: [Type]) (target :: Type) :: Constraint where
  CanMake a '[] t =
    TypeError
      ( TL.Text "The constructor for "
          :$$: TL.Text ""
          :$$: (TL.Text "  " :<>: ShowType (Output t))
          :$$: TL.Text ""
          :$$: TL.Text "cannot be added to the registry because"
          :$$: TL.Text ""
          :$$: (TL.Text "  " :<>: ShowType (Output a))
          :$$: TL.Text ""
          :$$: TL.Text " is not one of the registry outputs"
          :$$: TL.Text ""
          :$$: (TL.Text "The full constructor type for " :<>: ShowType (Output t) :<>: TL.Text " is")
          :$$: TL.Text ""
          :$$: ShowType t
          :$$: TL.Text ""
      )
  CanMake a (a ': _els) _t = ()
  CanMake a (_b ': els) t = CanMake a els t
