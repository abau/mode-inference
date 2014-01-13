{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
module ModeInference.Inference 
  (inference)
where

import           Control.Exception (assert)
import           Control.Monad.RWS.Strict
import           Data.Maybe (fromJust,mapMaybe)
import           Data.List (find)
import qualified Data.Map as M
import           ModeInference.Language
import           ModeInference.Syntax hiding (modeInstances)
import           ModeInference.Type hiding (mtypeOf)
import           ModeInference.Util
import           ModeInference.Constraint

inference :: Program Type -> (Program MType, [MTypeConstraint])
inference program = (program', constraints)
  where
    Program main decls = program
    adts               = mapMaybe (\case DeclAdt adt -> Just adt
                                         _           -> Nothing) decls

    (_, InferenceOutput bindings' constraints) = 
      execRWS (fromInfer $ inferMain main) 
              (emptyInferenceStack program) emptyInferenceState

    isMain (Binding (AnnIdentifier "main" _) _ _) = True
    isMain _                                      = False

    program' = Program (fromJust $ find isMain bindings')
             $ (map DeclAdt adts) 
            ++ (map DeclBind (filter (not . isMain)  bindings'))

type ModeInstanceNames = M.Map (Identifier,[MType]) Identifier

data InferenceState = InferenceState {
    modeInstances     :: ModeInstances
  , modeInstanceNames :: ModeInstanceNames
  , counter           :: Integer
}
emptyInferenceState = InferenceState M.empty M.empty 0

data InferenceStack = InferenceStack {
    varEnvironment :: M.Map Identifier MType
  , program        :: Program Type
}
emptyInferenceStack = InferenceStack M.empty

data InferenceOutput = InferenceOutput {
    instanceBindings :: [Binding MType]
  , constraints      :: [MTypeConstraint]
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

inferMType :: Type -> Infer MType
inferMType (Type id ts) = do
  modeVar <- newInt >>= \int -> return $ '_' : (show int)
  ts'     <- forM ts inferMType
  return $ MType id (ModeVar modeVar) ts'

inferMain :: Binding Type -> Infer MType
inferMain binding = argMTypes >>= inferBinding binding 
  where
    argMTypes = forM (map annIdAnnotation $ bindParameters binding) inferMType

inferBinding :: Binding Type -> [MType] -> Infer MType
inferBinding binding argMTypes =
  gets (M.lookup (annId $ bindName binding, argMTypes) . modeInstances) >>= \case
    Nothing -> inferNewBinding binding argMTypes
    Just m | null argMTypes -> return m
    Just m                  -> return $ MType "->" Known $ argMTypes ++ [m]

inferNewBinding :: Binding Type -> [MType] -> Infer MType
inferNewBinding binding argMTypes = assert (length argMTypes == length paramNames) $ do

  instanceName <- if bindingName == "main" 
                  then return "main"
                  else newInt >>= \n -> return $ bindingName ++ "_" ++ (show n)

  resultMType  <- inferMType $ resultType $ annIdAnnotation $ bindName binding 

  let instanceMType = if isConstant then resultMType
                      else MType "->" Known $ argMTypes ++ [resultMType]

  modify $ updateState instanceName resultMType

  (expM, expMType) <- local updateStack $ inferExpression 
                                        $ bindExpression binding

  tell $ InferenceOutput 
       [ Binding (AnnIdentifier instanceName instanceMType) instanceParams expM ]
       [ MTypeEq resultMType expMType ]

  return instanceMType
  
  where
    isConstant     = null argMTypes 
    bindingName    = annId $ bindName binding
    paramNames     = map annId $ bindParameters binding
    instanceParams = zipWith AnnIdentifier paramNames argMTypes

    updateState instanceName resultMType state = 
      state { modeInstances     = M.insert (bindingName,argMTypes) resultMType 
                                $ modeInstances state
            , modeInstanceNames = M.insert (bindingName,argMTypes) instanceName
                                $ modeInstanceNames state
            }

    updateStack stack = 
      stack { varEnvironment = M.union (M.fromList $ zip paramNames argMTypes)
                                       (varEnvironment stack)
            }

inferExpression :: Expression Type -> Infer (Expression MType, MType)
inferExpression expression = case expression of
  ExpVar v -> do
    mtype <- asks (fromJust . M.lookup (annId v) . varEnvironment) 
    return (ExpVar $ AnnIdentifier (annId v) mtype, mtype)

  ExpCon (AnnIdentifier c (Type t [])) -> 
    let mtype = MType t Known []
    in
      return (ExpCon $ AnnIdentifier c mtype, mtype)

  ExpApp (ExpVar v) args -> do
    (args', args'MTypes) <- forM args inferExpression >>= return . unzip

    binding <- asks $ bindingFromName v . program
    vMType  <- inferBinding binding args'MTypes 
    vName   <- gets (fromJust . M.lookup (annId v, args'MTypes) . modeInstanceNames) 
    return ( ExpApp (ExpVar $ AnnIdentifier vName vMType) args'
           , resultMType vMType)

  ExpCase d branches -> do
    (d',dMType)               <- inferExpression d
    (branches', branchMTypes) <- forM branches (inferBranch dMType) >>= return . unzip
    caseMType                 <- inferMType $ unmode $ head branchMTypes

    tell $ InferenceOutput [] [MTypeCase caseMType dMType branchMTypes]

    return (ExpCase d' branches', caseMType)

inferBranch :: MType -> Branch Type -> Infer (Branch MType, MType)
inferBranch dMType (Branch pat exp) = do
  pat'             <- inferPattern
  (exp', expMType) <- local (updateStack pat') $ inferExpression exp
  return (Branch pat' exp', expMType)
  where
    inferPattern = case pat of
      PatVar v    -> return $ PatVar $ AnnIdentifier (annId v) dMType
      PatCon c vs -> do
        adt         <- asks $ adtFromConstructorName (AnnIdentifier c undefined) . program
        constructor <- asks $ constructorFromName    (AnnIdentifier c undefined) . program

        let mtypes = map (getMType adt constructor) [0..]

        return $ PatCon c $ zipWith AnnIdentifier (map annId vs) mtypes
        where
          getMType adt constructor i =
            case adtVarIndexByConstructorArgIndex adt constructor i of
              Nothing -> error "Inference.inferBranch: not implemented"
              Just n  -> nthSubMType n dMType

    updateStack pat' stack = 
      stack { varEnvironment = M.union (M.fromList newVarBindings) $ varEnvironment stack }
      where
        newVarBindings = case pat' of
          PatVar v    -> [(annId v, annIdAnnotation v)]
          PatCon _ vs -> map (\v -> (annId v, annIdAnnotation v)) vs
