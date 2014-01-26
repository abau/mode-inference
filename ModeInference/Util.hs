module ModeInference.Util where

import Control.Exception (assert)
import Data.Generics
import Data.List (find,elemIndex)
import ModeInference.Language

import Debug.Trace
import ModeInference.PPrint

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

submode :: Adt -> Constructor -> Int -> Mode -> Mode
submode adt con n mode = assert (n < (length $ conParameters con)) $
  if isFixpoint (conParameters con !! n) adt
  then assert (n < length conModes) $
       case conModes !! n == ModeFixpoint of
          True -> mode
          _    -> error "Util.submode: missing fixpoint mode"
  else assert (n < length conModes) $ conModes !! n
  where
    conIndex = constructorIndex adt con
    conModes = submodes mode !! conIndex

replaceSubMode :: Adt -> Constructor -> Int -> Mode -> Mode -> Mode
replaceSubMode adt con n mode mode' = traceShow (pprint adt, pprint con,n,pprint mode) $
                                      assert (n < (length $ conParameters con)) $
  if isFixpoint (conParameters con !! n) adt
  then case submodes mode !! conIndex !! n == ModeFixpoint of
          True -> mode'
          _    -> error "Util.replaceSubMode: missing fixpoint mode"
  else mode { submodes = replaceInList conIndex (submodes mode) $ \mss ->
                          replaceInList n mss $ const mode' }
  where
    conIndex = constructorIndex adt con

    replaceInList 0 (x:xs) f = f x : xs
    replaceInList i (x:xs) f = x   : (replaceInList (i-1) xs f)

makeKnown :: Program Type -> Type -> MType
makeKnown program type_ = AnnotatedType (typeIdentifier type_) $ modeFromType type_
  where
    modeFromType t = Mode Known $ map modeFromConstructor 
                                $ adtConstructors 
                                $ adtFromName (typeIdentifier t) program
    modeFromConParam t = 
      if t == type_
      then ModeFixpoint
      else modeFromType t

    modeFromConstructor = map modeFromConParam . conParameters
