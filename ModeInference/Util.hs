module ModeInference.Util where

import Control.Exception (assert)
import Data.Generics
import Data.List (find,elemIndex)
import ModeInference.Language

hasFixpoint :: Adt -> Bool
hasFixpoint adt = any (any (flip isFixpoint adt) . conParameters) 
                $ adtConstructors adt

isFixpoint :: AnnotatedType a -> Adt -> Bool
isFixpoint t adt = (typeIdentifier t) == (adtName adt)

adtFromName :: (Data a, Typeable a) => Identifier -> Program a -> Adt
adtFromName id program = adt
  where
    [adt]  = everything (++) (mkQ [] go) program
    go adt = if adtName adt == id
             then [adt]
             else []

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

    go con@(Constructor name _) = if identifier id == name 
                                  then [con] 
                                  else []

constructorIndex :: Adt -> Constructor -> Int
constructorIndex adt con = case con `elemIndex` adtConstructors adt of
  Nothing -> error "Util.constructorIndex"
  Just i  -> i

bindingFromName :: (Data a, Typeable a) => TypedIdentifier a -> Program a -> Binding a
bindingFromName id program = binding
  where
    [binding]                = everything (++) (mkQ [] go) program
    go b@(Binding {}) = 
      if identifier (bindName b) == identifier id then [b] else []

subMode :: Adt -> Constructor -> Int -> Mode -> Mode
subMode adt con n mode = assert (n < (length $ conParameters con)) $
  if isFixpoint (conParameters con !! n) adt
  then mode
  else assert (n < length conModes) $ conModes !! n
  where
    conIndex = constructorIndex adt con
    conModes = subModes mode !! conIndex

replaceSubMode :: Adt -> Constructor -> Int -> Mode -> Mode -> Mode
replaceSubMode adt con n mode mode' = assert (n < (length $ conParameters con)) $
  if isFixpoint (conParameters con !! n) adt
  then mode'
  else mode { subModes = replaceInList conIndex (subModes mode) $ \mss ->
                          replaceInList n mss $ const mode' }
  where
    conIndex = constructorIndex adt con

    replaceInList 0 (x:xs) f = f x : xs
    replaceInList i (x:xs) f = x   : (replaceInList (i-1) xs f)

makeKnown :: Program Type -> Type -> MType
makeKnown program t = AnnotatedType (typeIdentifier t) $ modeFromType t
  where
    modeFromType t = Mode Known $ map modeFromConstructor 
                                $ adtConstructors 
                                $ adtFromName (typeIdentifier t) program

    modeFromConstructor = map modeFromType . conParameters
