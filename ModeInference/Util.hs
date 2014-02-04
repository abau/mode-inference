module ModeInference.Util where

import Control.Exception (assert)
import Control.Monad.Identity
import Data.Generics
import Data.List (find,findIndex,elemIndex)
import Data.Maybe (mapMaybe)
import ModeInference.Language

allEqual :: Eq a => [a] -> Bool
allEqual [_]      = True
allEqual (x:y:zs) = (x == y) && allEqual (y:zs)

isConstantMode :: Mode -> Bool
isConstantMode mode = case mode of 
  ModeVar {} -> False
  _          -> True

isRecursive :: Adt -> Bool
isRecursive = any (any isSelf . conParameters) . adtConstructors
  where
    isSelf TypeSelf = True
    isSelf _        = False

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
    binding = case everything (++) (mkQ [] go) program of
      [b] -> b
      [] -> error $ "Util.bindingFromName: '" ++ show (identifier id) ++ "' not found"
      _  -> error $ "Util.bindingFromName: '" ++ show (identifier id) ++ "' is ambiguous"

    go b@(Binding {}) = 
      if identifier (bindName b) == identifier id then [b] else []

removeBinding :: Identifier -> Program a -> Program a 
removeBinding name (Program main decl) = Program main $ mapMaybe go decl
  where
    go (DeclAdt adt) = Just $ DeclAdt adt
    go (DeclBind b)  = if identifier (bindName b) == name 
                       then Nothing
                       else Just $ DeclBind b

subtype :: Identifier -> Int -> MType -> MType
subtype conName j mtype@(MType _ _ cons) = assert (j < length params) $
  case params !! j of
    MTypeSelf -> mtype
    sub       -> sub
  where
    Just (MTypeConstructor _ params) = find ((conName ==) . mtypeConName) cons

replaceSubtype :: Identifier -> Int -> MType -> MType -> MType
replaceSubtype conName j mtype' (MType id mode cons) = assert (j < length params) $
  case params !! j of
    MTypeSelf -> mtype'
    _         -> MType id mode cons'
  where
    Just i                        = findIndex ((conName ==) . mtypeConName) cons
    MTypeConstructor conId params = cons !! i
    params'                       = replaceInList j mtype' params
    cons'                         = replaceInList i (MTypeConstructor conId params') cons 
    replaceInList _ _ []          = error "Util.replaceSubtype"
    replaceInList 0 y (_:xs)      = y : xs
    replaceInList i y (x:xs)      = x : (replaceInList (i-1) y xs)

makeKnown,makeUnknown :: Program Type -> Type -> MType 
makeKnown   = makeConstantMType Known
makeUnknown = makeConstantMType Unknown

makeConstantMType :: Mode -> Program Type -> Type -> MType
makeConstantMType mode program type_ = runIdentity $ makeMType (return mode) program type_

makeMType :: Monad m => m Mode -> Program Type -> Type -> m MType
makeMType makeMode program type_ = case type_ of
  FunctionType as r -> do
    as' <- forM as $ makeMType makeMode program
    r'  <- makeMType makeMode program r
    return $ FunctionMType as' r'

  TypeSelf -> return MTypeSelf

  Type id -> mtypeFromAdt $ adtFromName id program
  where
    mtypeFromAdt (Adt id cons) = do
      mode  <- makeMode
      cons' <- mapM makeMTypeCons cons
      return $ MType id mode cons'
    
    makeMTypeCons (Constructor id ps) = do
      ps' <- mapM (makeMType makeMode program) ps
      return $ MTypeConstructor id ps'

makeMonotoneMTypes :: Program Type -> Type -> [MType]
makeMonotoneMTypes program = toMonotoneMTypes . makeUnknown program

toMonotoneMTypes :: MType -> [MType]
toMonotoneMTypes = go . toMaxUnknown 
  where
    go MTypeSelf           = [MTypeSelf]
    go t@(MType id _ cons) = t : (map (MType id Known) $ sequence $ map goCon cons)

    goCon :: MTypeConstructor -> [MTypeConstructor]
    goCon (MTypeConstructor id ts) = 
      map (MTypeConstructor id) $ sequence $ map go ts

toMaxUnknown :: (Data a, Typeable a) => a -> a
toMaxUnknown = everywhere $ mkT $ const Unknown 

toMaxKnown :: (Data a, Typeable a) => a -> a
toMaxKnown = everywhere $ mkT $ const Known 
