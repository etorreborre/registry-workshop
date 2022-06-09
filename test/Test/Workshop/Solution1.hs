{-# LANGUAGE PolyKinds #-}

module Test.Workshop.Solution1 where

import Data.Dynamic
import GHC.Exts
import Protolude hiding (TypeRep, typeOf, typeRep)
import Type.Reflection

data Registry
  = Registry [Function]

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

-- | Apply a Dynamic function to a list of Dynamic values
applyFunction :: Dynamic -> [Dynamic] -> Either Text Dynamic
applyFunction _f [] = Left "no parameters to apply"
applyFunction f [i] = applyParameter f i
applyFunction f (i : is) = do
  f' <- applyParameter f i
  applyFunction f' is

-- | Apply a Dynamic parameter to a Dynamic function
applyParameter :: Dynamic -> Dynamic -> Either Text Dynamic
applyParameter f i = maybe (Left $ "failed to apply " <> show i <> " to : " <> show f) Right (dynApply f i)

infixr 5 +:

(+:) :: forall a. (Typeable a) => a -> Registry -> Registry
(+:) a (Registry as) = Registry (createFunction a : as)

make :: forall a. (Typeable a) => Registry -> Either Text a
make r = do
  let targetType = show $ typeRep @a
  evalStateT (makeValue targetType r r) [] >>= \v ->
    case fromDynamic v of
      Just a -> Right a
      Nothing -> Left $ "cannot make a " <> show targetType

makeValue :: Text -> Registry -> Registry -> StateT [Value] (Either Text) Dynamic
makeValue targetType _ (Registry []) = lift $ Left $ "cannot make a " <> show targetType
makeValue targetType r (Registry (Function f (FunctionType ins out) : fs)) = do
  values <- get
  case findValue targetType values of
    Just v ->
      pure v
    Nothing ->
      case dynTypeRep f of
        -- a function with some arguments
        SomeTypeRep (Fun _ _out) ->
          if out == targetType
            then do
              inputValues <- for ins $ \i -> makeValue i r r
              -- make a new value here
              v <- lift $ applyFunction f inputValues
              modify (Value v (show $ dynTypeRep v) :)
              pure v
            else makeValue targetType r (Registry fs)
        -- a function with no arguments
        SomeTypeRep _out ->
          if out == targetType
            then pure f
            else makeValue targetType r (Registry fs)

findValue :: Text -> [Value] -> Maybe Dynamic
findValue _ [] = Nothing
findValue targetType ((Value d t) : vs) =
  if targetType == t then Just d else findValue targetType vs
