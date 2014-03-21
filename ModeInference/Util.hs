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

isRecursive :: Adt -> Bool
isRecursive = everything (||) $ mkQ False isSelf
  where
    isSelf (TypeSelf _) = True
    isSelf _            = False

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

validReferences :: MType -> Bool
validReferences = go [] (-1)
  where
    go s l t@(MType _ _ cons)   = (not $ t `elem` s) && (all (goCons (t:s) l) cons)
    go s l (FunctionMType as r) = (all (go s l) as) && (go s l r)
    go _ l (MTypeSelf l')       = l' <= l

    goCons s l (MTypeConstructor _ ts) = all (go s $ l + 1) ts

subtype :: Identifier -> Int -> MType -> MType
subtype conName j (MType _ _ cons) = assert (j < length params)
                                   $ params !! j
  where
    Just (MTypeConstructor _ params) = find ((conName ==) . mtypeConName) cons

recursiveSubtype :: Identifier -> Int -> MType -> MType
recursiveSubtype conName j mtype = case subtype conName j mtype of
  MTypeSelf 0 -> mtype
  MTypeSelf _ -> error "Util.recursiveSubtype"
  sub         -> fixMonotonicity $ replaceReferences 0 sub
  where
    replaceReferences level = go
      where
        go (MType i m cons)           = MType i m $ map goCon cons
        go (MTypeSelf l) | level == l = replaceSubtype conName j (MTypeSelf l) mtype
        go (MTypeSelf l)              = MTypeSelf l

        goCon (MTypeConstructor n ts) = MTypeConstructor n
                                      $ map (replaceReferences $ level + 1) ts

    fixMonotonicity = go Known
      where
        go minMode (MType i m cons) = case minMode of
          Known     -> MType i m       $ map (goCon m      ) cons
          Unknown   -> MType i Unknown $ map (goCon Unknown) cons

        go _ (MTypeSelf l) = MTypeSelf l

        goCon minMode (MTypeConstructor n ts) = MTypeConstructor n $ map (go minMode) ts

replaceSubtype :: Identifier -> Int -> MType -> MType -> MType
replaceSubtype conName j newSubType mtype@(MType id mode cons) = assert (j < length params) $
  case params !! j of
    MTypeSelf 0 -> case newSubType of
      MTypeSelf 0 -> mtype
      MTypeSelf _ -> error "Util.replaceSubtype"
      _           -> newSubType

    MTypeSelf _ -> error "Util.replaceSubtype"
    _           -> MType id mode cons'
  where
    Just i                        = findIndex ((conName ==) . mtypeConName) cons
    MTypeConstructor conId params = cons !! i
    params'                       = replaceInList j newSubType params
    cons'                         = replaceInList i (MTypeConstructor conId params') cons 
    replaceInList _ _ []          = error "Util.replaceSubtype.replaceInList"
    replaceInList 0 y (_:xs)      = y : xs
    replaceInList i y (x:xs)      = x : (replaceInList (i-1) y xs)

makeKnown,makeUnknown :: Program Type -> Type -> MType 
makeKnown   = makeConstantMType Known
makeUnknown = makeConstantMType Unknown

makeConstantMType :: Mode -> Program Type -> Type -> MType
makeConstantMType mode program type_ = runIdentity $ makeMType (return mode) program type_

makeMType :: Monad m => m Mode -> Program Type -> Type -> m MType
makeMType makeMode program = go []
  where 
    go [] (FunctionType as r) = do
      as' <- forM as $ go []
      r'  <- go [] r
      return $ FunctionMType as' r'

    go stack type_@(Type id args) = 
      case type_ `elemIndex` stack of
      Just l  -> return $ MTypeSelf l
      Nothing -> mtypeFromAdt $ adtFromName id program
      where
        stack' = type_ : stack
        mtypeFromAdt (Adt _ vars cons) = do
          mode  <- makeMode
          cons' <- mapM makeMTypeCons cons
          return $ MType id mode cons'
          where
            makeMTypeCons (Constructor id cs) = do
              cs' <- mapM makeMTypeConParam cs
              return $ MTypeConstructor id cs'

            makeMTypeConParam (ConParamType t) = go stack' t 
            makeMTypeConParam (ConParamVar v)  = do 
              let Just i = v `elemIndex` vars
              go stack' $ args !! i

    go _ (TypeSelf l) = return $ MTypeSelf l

makeMonotoneMTypes :: Program Type -> Type -> [MType]
makeMonotoneMTypes program = toMonotoneMTypes . makeUnknown program

toMonotoneMTypes :: MType -> [MType]
toMonotoneMTypes = go . toMaxUnknown
  where
    go (MTypeSelf i)       = [MTypeSelf i]
    go t@(MType id _ cons) = t : (map (MType id Known) $ sequence $ map goCon cons)

    goCon :: MTypeConstructor -> [MTypeConstructor]
    goCon (MTypeConstructor id ts) = map (MTypeConstructor id) $ sequence $ map go ts

toMaxUnknown :: MType -> MType
toMaxUnknown = everywhere $ mkT $ const Unknown 

toMaxKnown :: MType -> MType
toMaxKnown = everywhere $ mkT $ const Known 

instantiateMType :: Monad m => m Mode -> MType -> m MType
instantiateMType f = everywhereM $ mkM $ const f

recursiveArguments :: Program Type -> TypedIdentifier Type -> [MType] -> ([MType],[MType])
recursiveArguments program constructor = 
  recursiveArguments' (identifier constructor) (makeKnown program resultT)
  where
    FunctionType _ resultT = idType constructor

recursiveArguments' :: Identifier -> MType -> [MType] -> ([MType], [MType])
recursiveArguments' constructorName resultT argTs = (concat recursions, argTs')
  where
    paramTs             = map (\i -> subtype constructorName i resultT) [0..]
    (recursions,argTs') = unzip $ zipWith (go 0) paramTs argTs
    
    go _     (MTypeSelf l) (MTypeSelf l') = assert (l == l')    $ ([] , MTypeSelf l')
    go level (MTypeSelf l) t              = assert (l == level) $ ([t], MTypeSelf l)

    go level (MType i1 _ cs1) (MType i2 m2 cs2) = assert (i1 == i2) 
                                                $ assert (length cs1 == length cs2)
                                                $ ( concat recs
                                                  , MType i2 m2 cs2' )
      where
        (recs, cs2') = unzip $ zipWith (goCon level) cs1 cs2

    goCon level (MTypeConstructor i1 ps1) (MTypeConstructor i2 ps2) = 
        assert (i1 == i2) 
      $ assert (length ps1 == length ps2) $ (concat recs, MTypeConstructor i2 ps2')
      where
        (recs, ps2') = unzip $ zipWith (go $ level + 1) ps1 ps2
