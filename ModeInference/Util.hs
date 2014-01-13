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

adtFromConstructorName :: (Data a, Typeable a) => AnnIdentifier a -> Program a -> Adt
adtFromConstructorName name program = adt
  where
    [adt] = everything (++) (mkQ [] go) program

    go adt = case find matchingConstructor (adtConstructors adt) of
      Nothing -> []
      Just _  -> [adt]

    matchingConstructor (Constructor name' _) = annId name == name'

constructorFromName :: (Data a, Typeable a) => AnnIdentifier a -> Program a -> Constructor
constructorFromName name program = con
  where
    [con]                    = everything (++) (mkQ [] go) program
    go con@(Constructor name' _) = 
      if annId name == name' then [con] else []

adtVarIndexByConstructorArgIndex :: Adt -> Constructor -> Int -> Maybe Int
adtVarIndexByConstructorArgIndex adt con n = assert (n < length (conArguments con))
                                 $ case conArg of
  ConsArgRec   -> Nothing
  ConsArgVar v -> case v `elemIndex` adtVariables adt of
                    Nothing -> error "adtVarIndexByConstructorArgIndex"
                    result  -> result
  where
    conArg = conArguments con !! n

modeOf :: MType -> Mode
modeOf (MType _ m _) = m

subMTypes :: MType -> [MType]
subMTypes (MType _ _ ts) = ts

nthSubMType :: Int -> MType -> MType
nthSubMType n (MType _ _ ts) = assert (n < length ts) $ ts !! n

bindingFromName :: (Data a, Typeable a) => AnnIdentifier a -> Program a -> Binding a
bindingFromName name program = binding
  where
    [binding]                = everything (++) (mkQ [] go) program
    go b@(Binding {}) = 
      if annId (bindName b) == annId name then [b] else []

makeMaxUnknown :: MType -> MType
makeMaxUnknown = everywhere $ mkT $ const Unknown

constantMode :: Mode -> Bool
constantMode Known       = True
constantMode Unknown     = True
constantMode (ModeVar _) = False
