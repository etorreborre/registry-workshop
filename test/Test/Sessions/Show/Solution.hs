{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}

module Test.Sessions.Show.Solution where

import Data.Dynamic
import qualified Data.Text as T
import GHC.Exts
import Protolude hiding (TypeRep, typeOf, typeRep)
import Type.Reflection

data Registry
  = Registry [Value] [Function]

end :: Registry
end = Registry mempty mempty

data Typed a
  = TypedValue Value
  | TypedFunction Function

fun :: Typeable a => a -> Typed a
fun = TypedFunction . createFunction

val :: (Typeable a, Show a) => a -> Typed a
val = TypedValue . createValue

infixr 5 +:

(+:) :: forall a. (Typeable a) => Typed a -> Registry -> Registry
(+:) (TypedValue v) (Registry vs fs) = Registry (v : vs) fs
(+:) (TypedFunction f) (Registry vs fs) = Registry vs (f : fs)

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

-- * DISPLAY

display :: Registry -> Text
display (Registry values functions) =
  T.unlines $
    ["VALUES"]
      <> (displayValue <$> values)
      <> [""]
      <> ["FUNCTIONS"]
      <> (displayFunction <$> functions)

displayValue :: Value -> Text
displayValue (Value _ valueType Nothing) = show valueType
displayValue (Value _ valueType (Just v)) = v <> "::" <> valueType

displayFunction :: Function -> Text
displayFunction (Function _ (FunctionType ins out)) =
  T.intercalate " -> " (ins <> [out])
