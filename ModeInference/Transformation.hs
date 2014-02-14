{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
module ModeInference.Transformation
  (transform)
where

import           Control.Exception (assert)
import           Data.Maybe (fromJust,mapMaybe)
import qualified Data.Map as M
import           Data.List (find)
import           Control.Monad.RWS.Strict
import           ModeInference.Language
import           ModeInference.Syntax hiding (modeInstances)
import           ModeInference.Inference (inferBinding,inferConstructorApp,inferPattern)
import           ModeInference.Type (mtypeOf)
import           ModeInference.Util

transform :: Program Type -> [MType] -> Program MType
transform program argTypes = program'
  where
    Program main decls = program
    adts               = mapMaybe (\case DeclAdt adt -> Just adt
                                         _           -> Nothing) decls

    (_, bindings') = execRWS (fromTransform $ transformBinding main argTypes) 
                             (emptyEnvironment program) emptyState

    isMain (Binding (TypedIdentifier "main" _) _ _) = True
    isMain _                                        = False

    program' = Program (fromJust $ find isMain bindings')
             $ (map DeclAdt adts) 
            ++ (map DeclBind (filter (not . isMain)  bindings'))

type ModeInstanceNames = M.Map (Identifier,[MType]) Identifier

data State = State {
    modeInstances     :: ModeInstances
  , modeInstanceNames :: ModeInstanceNames
}
emptyState = State M.empty M.empty

data Environment = Environment {
    envVarBindings :: M.Map Identifier MType
  , envProgram     :: Program Type
}
emptyEnvironment = Environment M.empty

type Output = [Binding MType]

newtype Transform a = Transform { 
    fromTransform :: RWS Environment Output State a
  }
  deriving (Monad, MonadReader Environment, MonadWriter Output, MonadState State)

transformBinding :: Binding Type -> [MType] -> Transform MType
transformBinding binding argTypes =
  gets (M.lookup key . modeInstances) >>= \case
    Nothing -> transformNewBinding binding argTypes
    Just m | null argTypes -> return m
    Just m                 -> return $ FunctionMType argTypes m
  where
    key = (identifier $ bindName binding, argTypes)

transformNewBinding :: Binding Type -> [MType] -> Transform MType
transformNewBinding binding argTypes = assert (length argTypes == length paramNames) $ do
  instanceName <- gets $ modeInstanceName bindingName . M.size . modeInstances
  resultType   <- asks envProgram >>= \p -> return $ inferBinding p binding argTypes 

  let instanceType = if isConstant then resultType
                     else FunctionMType argTypes resultType

  modify $ updateState instanceName resultType

  exp' <- local updateEnv $ transformExpression $ bindExpression binding

  tell [ Binding (TypedIdentifier instanceName instanceType) instanceParams exp' ]
  return instanceType
  where
    isConstant     = null argTypes 
    bindingName    = identifier $ bindName binding
    paramNames     = map identifier $ bindParameters binding
    instanceParams = zipWith TypedIdentifier paramNames argTypes

    updateState instanceName resultType state = 
      state { modeInstances     = M.insert (bindingName,argTypes) resultType 
                                $ modeInstances state
            , modeInstanceNames = M.insert (bindingName,argTypes) instanceName
                                $ modeInstanceNames state
            }

    updateEnv env = 
      env { envVarBindings = M.union (M.fromList $ zip paramNames argTypes)
                                     (envVarBindings env)
          }

transformExpression :: Expression Type -> Transform (Expression MType)
transformExpression expression = case expression of
  ExpVar v -> do
    mtype <- asks $ \env -> case M.lookup (identifier v) (envVarBindings env) of
              Nothing -> error $ "Transformation.transformExpression: '" ++ (identifier v) ++ "' not found"
              Just t  -> t
      
    return $ ExpVar $ TypedIdentifier (identifier v) mtype

  ExpCon (TypedIdentifier c t) -> do
    program <- asks envProgram
    return $ ExpCon $ TypedIdentifier c $ makeKnown program t

  ExpApp (ExpVar v) args -> do
    args' <- forM args transformExpression

    let arg'Types = map mtypeOf args'

    binding <- asks $ bindingFromName v . envProgram
    vType   <- transformBinding binding arg'Types 

    v'      <- gets $ \s -> case M.lookup (identifier v, arg'Types) (modeInstanceNames s) of
                  Nothing   -> error $ "Transformation.transformExpression: '" ++ (identifier v) ++ "' not found"
                  Just name -> name

    return $ ExpApp (ExpVar $ TypedIdentifier v' vType) args'

  ExpApp (ExpCon c) args -> do
    args' <- forM args transformExpression

    let arg'Types = map mtypeOf args'

    resultType <- asks $ \env -> inferConstructorApp (envProgram env) c arg'Types 

    let cType = FunctionMType arg'Types resultType

    return $ ExpApp (ExpCon $ c { idType = cType }) args'

  ExpCase d branches -> do
    d'        <- transformExpression d
    branches' <- forM branches $ inferBranch $ mtypeOf d'
    return $ ExpCase d' branches'

  ExpLet (Binding name [] value) exp -> do
    value' <- transformExpression value
    let value'Type = mtypeOf value'
        name'      = name { idType = value'Type }
    exp'   <- local (updateEnv value'Type) $ transformExpression exp
    return $ ExpLet (Binding name' [] value') exp'
    where
      updateEnv value'Type env = 
        env { envVarBindings = M.insert (identifier name) value'Type $ envVarBindings env }

inferBranch :: MType -> Branch Type -> Transform (Branch MType)
inferBranch dType (Branch pat exp) = do
  exp' <- local updateEnv $ transformExpression exp
  return $ Branch pat' exp'
  where
    pat'          = inferPattern dType pat 
    updateEnv env = 
      env { envVarBindings = M.union (M.fromList newVarBindings) $ envVarBindings env }
      where
        newVarBindings = case pat' of
          PatVar v    -> [(identifier v, idType v)]
          PatCon _ vs -> map (\v -> (identifier v, idType v)) vs
