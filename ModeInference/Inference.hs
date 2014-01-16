{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
module ModeInference.Inference 
  (ModeInstanceNames, inference)
where

import           Control.Exception (assert)
import           Control.Monad.RWS.Strict
import           Data.Maybe (fromJust,mapMaybe)
import           Data.List (find)
import qualified Data.Map as M
import           ModeInference.Language
import           ModeInference.Syntax (unmode)
import           ModeInference.Type hiding (mtypeOf)
import           ModeInference.Util
import           ModeInference.Constraint

type ModeInstances     = M.Map (Identifier,[IMType]) IMType
type ModeInstanceNames = M.Map (Identifier,[IMType]) Identifier

inference :: Program Type -> (Program IMType, [IMTypeConstraint], ModeInstanceNames)
inference program = (program', constraints, modeInstanceNames)
  where
    Program main decls = program
    adts               = mapMaybe (\case DeclAdt adt -> Just adt
                                         _           -> Nothing) decls

    (InferenceState _ modeInstanceNames _, InferenceOutput bindings' constraints) = 
      execRWS (fromInfer $ inferMain main) 
              (emptyInferenceStack program) emptyInferenceState

    isMain (Binding (TypedIdentifier "main" _) _ _) = True
    isMain _                                        = False

    program' = Program (fromJust $ find isMain bindings')
             $ (map DeclAdt adts) 
            ++ (map DeclBind (filter (not . isMain)  bindings'))

data InferenceState = InferenceState {
    modeInstances     :: ModeInstances
  , modeInstanceNames :: ModeInstanceNames
  , counter           :: Integer
}
emptyInferenceState = InferenceState M.empty M.empty 0

data InferenceStack = InferenceStack {
    varEnvironment :: M.Map Identifier IMType
  , program        :: Program Type
}
emptyInferenceStack = InferenceStack M.empty

data InferenceOutput = InferenceOutput {
    instanceBindings :: [Binding IMType]
  , constraints      :: [IMTypeConstraint]
}

instance Monoid InferenceOutput where
  mempty      = InferenceOutput [] []
  mappend a b = InferenceOutput (mappend (instanceBindings a) (instanceBindings b))
                                (mappend (constraints a)      (constraints b))

newtype Infer a = Infer { fromInfer :: RWS InferenceStack InferenceOutput InferenceState a }
  deriving ( Monad, MonadReader InferenceStack, MonadWriter InferenceOutput
           , MonadState InferenceState)

newInt :: Infer Integer
newInt = do 
  c <- gets counter
  modify $ \s -> s { counter = counter s + 1 }
  return c

inferIMType :: Type -> Infer IMType
inferIMType (AnnotatedType id () ts) = do
  modeVar <- newInt >>= \int -> return $ '_' : (show int)
  ts'     <- forM ts inferIMType
  return $ AnnotatedType id (IMVar modeVar) ts'

inferMain :: Binding Type -> Infer IMType
inferMain binding = argIMTypes >>= inferBinding binding 
  where
    argIMTypes = forM (map idType $ bindParameters binding) inferIMType

inferBinding :: Binding Type -> [IMType] -> Infer IMType
inferBinding binding argIMTypes =
  gets (M.lookup (identifier $ bindName binding, argIMTypes) . modeInstances) >>= \case
    Nothing -> inferNewBinding binding argIMTypes
    Just m | null argIMTypes -> return m
    Just m                   -> return $ AnnotatedType "->" (Mode Known) 
                                       $ argIMTypes ++ [m]

inferNewBinding :: Binding Type -> [IMType] -> Infer IMType
inferNewBinding binding argIMTypes = assert (length argIMTypes == length paramNames) $ do

  instanceName <- if bindingName == "main" 
                  then return "main"
                  else newInt >>= \n -> return $ bindingName ++ "_" ++ (show n)

  resultIMType <- inferIMType $ resultType $ idType $ bindName binding 

  let instanceIMType = if isConstant then resultIMType
                       else AnnotatedType "->" (Mode Known) 
                                             $ argIMTypes ++ [resultIMType]

  modify $ updateState instanceName resultIMType

  (expM, expIMType) <- local updateStack $ inferExpression 
                                         $ bindExpression binding

  tell $ InferenceOutput 
       [ Binding (TypedIdentifier instanceName instanceIMType) instanceParams expM ]
       [ IMTypeEq resultIMType expIMType ]

  return instanceIMType
  
  where
    isConstant     = null argIMTypes 
    bindingName    = identifier $ bindName binding
    paramNames     = map identifier $ bindParameters binding
    instanceParams = zipWith TypedIdentifier paramNames argIMTypes

    updateState instanceName resultIMType state = 
      state { modeInstances     = M.insert (bindingName,argIMTypes) resultIMType 
                                $ modeInstances state
            , modeInstanceNames = M.insert (bindingName,argIMTypes) instanceName
                                $ modeInstanceNames state
            }

    updateStack stack = 
      stack { varEnvironment = M.union (M.fromList $ zip paramNames argIMTypes)
                                       (varEnvironment stack)
            }

inferExpression :: Expression Type -> Infer (Expression IMType, IMType)
inferExpression expression = case expression of
  ExpVar v -> do
    mtype <- asks (fromJust . M.lookup (identifier v) . varEnvironment) 
    return (ExpVar $ TypedIdentifier (identifier v) mtype, mtype)

  ExpCon (TypedIdentifier c (AnnotatedType t () [])) -> 
    let mtype = AnnotatedType t (Mode Known) []
    in
      return (ExpCon $ TypedIdentifier c mtype, mtype)

  ExpApp (ExpVar v) args -> do
    (args', args'IMTypes) <- forM args inferExpression >>= return . unzip

    binding <- asks $ bindingFromName v . program
    vIMType <- inferBinding binding args'IMTypes 
    vName   <- gets (fromJust . M.lookup (identifier v, args'IMTypes) 
                              . modeInstanceNames) 
    return ( ExpApp (ExpVar $ TypedIdentifier vName vIMType) args'
           , resultType vIMType)

  ExpApp (ExpCon c) args -> do
    (args', args'IMTypes) <- forM args inferExpression >>= return . unzip

    cIMType <- inferConstructorApp c args'IMTypes 
    return ( ExpApp (ExpCon $ c { idType = cIMType}) args'
           , resultType cIMType)

  ExpCase d branches -> do
    (d',dIMType)               <- inferExpression d
    (branches', branchIMTypes) <- forM branches (inferBranch dIMType) >>= return . unzip
    caseIMType                 <- inferIMType $ unmode $ head branchIMTypes

    tell $ InferenceOutput [] [IMTypeCase caseIMType dIMType branchIMTypes]

    return (ExpCase d' branches', caseIMType)

inferConstructorApp :: TypedIdentifier Type -> [IMType] -> Infer IMType
inferConstructorApp cId cArgIMTypes = do
  adt          <- asks $ adtFromConstructorName cId . program
  constructor  <- asks $ constructorFromName    cId . program
  resultIMType <- inferIMType $ resultType $ idType cId

  makeConstraints adt constructor resultIMType

  return $ AnnotatedType "->" (Mode Known) $ cArgIMTypes ++ [resultIMType]

  where 
    makeConstraints adt constructor resultIMType = 
      tell $ InferenceOutput [] $ concat
        [ map (uncurry IMTypeSup) $ M.toList argumentSuprema
        , if null resultSupremum 
          then [] 
          else [ IMTypeSup resultIMType $ resultSupremum ]
        ]
      where
        argumentSuprema :: M.Map IMType [IMType]
        argumentSuprema = M.fromListWith (++) $ mapMaybe argumentSupremum
                                              $ zip [0..] cArgIMTypes
        argumentSupremum (i,argIMType) =
          case adtVarIndexByConstructorArgIndex adt constructor i of
            Nothing -> Nothing
            Just n  -> Just (typeArguments resultIMType !! n, [argIMType])

        resultSupremum :: [IMType]
        resultSupremum = mapMaybe go $ zip [0..] cArgIMTypes
          where 
            go (i,argIMType) = case adtVarIndexByConstructorArgIndex adt constructor i of
              Nothing -> Just argIMType
              Just _  -> Nothing

inferBranch :: IMType -> Branch Type -> Infer (Branch IMType, IMType)
inferBranch dIMType (Branch pat exp) = do
  pat'              <- inferPattern
  (exp', expIMType) <- local (updateStack pat') $ inferExpression exp
  return (Branch pat' exp', expIMType)
  where
    inferPattern = case pat of
      PatVar v    -> return $ PatVar $ TypedIdentifier (identifier v) dIMType
      PatCon c vs -> do
        adt         <- asks $ adtFromConstructorName (TypedIdentifier c undefined) . program
        constructor <- asks $ constructorFromName    (TypedIdentifier c undefined) . program

        let mtypes = map (getIMType adt constructor) [0..]

        return $ PatCon c $ zipWith TypedIdentifier (map identifier vs) mtypes
        where
          getIMType adt constructor i =
            case adtVarIndexByConstructorArgIndex adt constructor i of
              Nothing -> dIMType
              Just n  -> typeArguments dIMType !! n

    updateStack pat' stack = 
      stack { varEnvironment = M.union (M.fromList newVarBindings) $ varEnvironment stack }
      where
        newVarBindings = case pat' of
          PatVar v    -> [(identifier v, idType v)]
          PatCon _ vs -> map (\v -> (identifier v, idType v)) vs
