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
import           ModeInference.Type
import           ModeInference.Util

inference :: Program Type -> [MType] -> Program MType
inference program mainArgMTypes = Program (fromJust $ find isMain bindings')
                                $ (map DeclAdt adts) 
                               ++ (map DeclBind (filter (not . isMain)  bindings'))
  where
    Program main decls = program
    adts               = mapMaybe (\case DeclAdt adt -> Just adt
                                         _           -> Nothing) decls

    (_, bindings') = execRWS (fromInfer $ inferBinding main mainArgMTypes) 
                             (emptyInferenceStack program) emptyInferenceState

    isMain (Binding (AnnIdentifier "main" _) _ _) = True
    isMain _                                      = False

data InferenceState = InferenceState {
    modeInstances     :: ModeInstances
  , modeInstanceNames :: M.Map (Identifier,[MType]) Identifier
}
emptyInferenceState = InferenceState M.empty M.empty

data InferenceStack = InferenceStack {
    currentInstance     :: (Identifier,[MType])
  , currentInstanceName :: Identifier
  , varEnvironment      :: M.Map Identifier MType
  , program             :: Program Type
}
emptyInferenceStack = InferenceStack undefined undefined M.empty

type InferenceDecls = [Binding MType]

newtype Infer a = Infer { fromInfer :: RWS InferenceStack InferenceDecls InferenceState a }
  deriving ( Monad, MonadReader InferenceStack, MonadWriter InferenceDecls
           , MonadState InferenceState)

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
                  else gets (M.size . modeInstances) 
                       >>= \n -> return $ bindingName ++ (show n)

  expM <- local (updateStack instanceName) $ inferExpression 
                                           $ bindExpression binding

  let expMType      = mtypeOf expM
      instanceMType = if isConstant then expMType
                      else MType "->" Known $ argMTypes ++ [expMType]

  modify $ updateState instanceName expMType

  tell [ Binding (AnnIdentifier instanceName instanceMType) instanceParams expM ]
  return instanceMType
  
  where
    isConstant     = null argMTypes 
    bindingName    = annId $ bindName binding
    paramNames     = map annId $ bindParameters binding
    instanceParams = zipWith AnnIdentifier paramNames argMTypes

    updateStack instanceName stack = 
      stack { currentInstance      = (bindingName, argMTypes)
            , currentInstanceName  = instanceName
            , varEnvironment       = M.union (M.fromList $ zip paramNames argMTypes)
                                             (varEnvironment stack)
            }

    updateState instanceName expMType state = 
      state { modeInstances     = M.insert (bindingName,argMTypes) expMType 
                                $ modeInstances state
            , modeInstanceNames = M.insert (bindingName,argMTypes) instanceName
                                $ modeInstanceNames state
            }

inferExpression :: Expression Type -> Infer (Expression MType)
inferExpression expression = case expression of
  ExpVar v -> do
    mtype <- asks (fromJust . M.lookup (annId v) . varEnvironment) 
    return $ ExpVar $ AnnIdentifier (annId v) mtype

  ExpCon (AnnIdentifier c (Type t [])) -> return $ ExpCon $ AnnIdentifier c $ MType t Known []

  ExpApp (ExpVar v) args -> do
    args' <- forM args inferExpression
    let args'MTypes = map mtypeOf args'

    binding <- asks $ bindingFromName v . program
    vMType  <- inferBinding binding args'MTypes 
    vName   <- gets (fromJust . M.lookup (annId v, args'MTypes) . modeInstanceNames) 
    return $ ExpApp (ExpVar $ AnnIdentifier vName vMType) args'

  ExpCase d branches -> do
    d'        <- inferExpression d
    branches' <- forM branches $ inferBranch d'
    return $ ExpCase d' branches'

inferBranch :: Expression MType -> Branch Type -> Infer (Branch MType)
inferBranch d (Branch pat exp) = do
  pat' <- inferPattern
  exp' <- local (updateStack pat') $ inferExpression exp
  return $ Branch pat' exp'
  where
    dMType       = mtypeOf d
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
