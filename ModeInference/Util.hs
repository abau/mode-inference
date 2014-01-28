module ModeInference.Util where

import Control.Exception (assert)
import Control.Monad.Identity
import Data.Generics
import Data.List (find,elemIndex)
import ModeInference.Language

allEqual :: Eq a => [a] -> Bool
allEqual [_]      = True
allEqual (x:y:zs) = (x == y) && allEqual (y:zs)

isConstantAtom :: ModeAtom -> Bool
isConstantAtom atom = case atom of 
  ModeVar {} -> False
  _          -> True

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
replaceSubMode adt con n mode mode' = assert (n < (length $ conParameters con)) $
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

makeKnown,makeUnknown :: Program Type -> Type -> MType 
makeKnown   = makeConstantMType Known
makeUnknown = makeConstantMType Unknown

makeConstantMType :: ModeAtom -> Program Type -> Type -> MType
makeConstantMType atom program type_ = runIdentity $ makeMType (return atom) program type_

makeMType :: Monad m => m ModeAtom -> Program Type -> Type -> m MType
makeMType makeAtom program type_ = case type_ of
  FunctionType as r -> do
    as' <- forM as $ makeMType makeAtom program
    r'  <- makeMType makeAtom program r
    return $ FunctionType as' r'

  AnnotatedType id _ -> modeFromType type_ >>= return . AnnotatedType id
  where
    modeFromType t = do
      atom  <- makeAtom
      cons' <- mapM modeFromConstructor $ adtConstructors 
                                        $ adtFromName (typeIdentifier t) program
      return $ Mode atom cons'
    
    modeFromConstructor = mapM modeFromConParam . conParameters

    modeFromConParam t = 
      if t == type_
      then return ModeFixpoint
      else modeFromType t

makeMonotoneMTypes :: Program Type -> Type -> [MType]
makeMonotoneMTypes program type_ = case type_ of
  AnnotatedType {} -> map (\m -> type_ { typeAnnotation = m }) $ go 
                                                               $ typeAnnotation 
                                                               $ makeUnknown program type_
  FunctionType {} -> error "Util.makeMonotoneMTypes"
  where
    go ModeFixpoint = [ModeFixpoint]
    go m            = m : (map (Mode Known) $ sequence $ map goCon $ submodes m)

    goCon :: [Mode] -> [[Mode]]
    goCon = sequence . map go

toMaxUnknown :: (Data a, Typeable a) => a -> a
toMaxUnknown = everywhere $ mkT $ const Unknown 

toMaxKnown :: (Data a, Typeable a) => a -> a
toMaxKnown = everywhere $ mkT $ const Known 
