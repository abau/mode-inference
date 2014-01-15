module ModeInference.Util where

import Control.Exception (assert)
import Data.Generics
import Data.List (find,elemIndex)
import ModeInference.Language

isRecursiveAdt :: Adt -> Bool
isRecursiveAdt = everything (||) $ mkQ False go
  where
    go ConsArgRec = True
    go _          = False

adtFromConstructorName :: (Data a, Typeable a) => TypedIdentifier a -> Program a -> Adt
adtFromConstructorName id program = adt
  where
    [adt] = everything (++) (mkQ [] go) program

    go adt = case find matchingConstructor (adtConstructors adt) of
      Nothing -> []
      Just _  -> [adt]

    matchingConstructor (Constructor name' _) = identifier id == name'

constructorFromName :: (Data a, Typeable a) => TypedIdentifier a -> Program a -> Constructor
constructorFromName id program = con
  where
    [con] = everything (++) (mkQ [] go) program

    go con@(Constructor name _) = 
      if identifier id == name then [con] else []

adtVarIndexByConstructorArgIndex :: Adt -> Constructor -> Int -> Maybe Int
adtVarIndexByConstructorArgIndex adt con n = assert (n < length (conArguments con))
                                 $ case conArg of
  ConsArgRec   -> Nothing
  ConsArgVar v -> case v `elemIndex` adtVariables adt of
                    Nothing -> error "adtVarIndexByConstructorArgIndex"
                    result  -> result
  where
    conArg = conArguments con !! n

bindingFromName :: (Data a, Typeable a) => TypedIdentifier a -> Program a -> Binding a
bindingFromName id program = binding
  where
    [binding]                = everything (++) (mkQ [] go) program
    go b@(Binding {}) = 
      if identifier (bindName b) == identifier id then [b] else []
