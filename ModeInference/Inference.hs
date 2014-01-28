{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
module ModeInference.Inference
  (inferMain, inferBinding, inferConstructorApp, inferPattern)
where

import           Control.Exception (assert)
import           Control.Monad.Reader
import qualified Data.Map as M
import           Data.Maybe (fromJust)
import           ModeInference.Language
import           ModeInference.Type
import           ModeInference.Util
import           ModeInference.Semantic
import           ModeInference.Syntax

inferMain :: Program Type -> [MType] -> MType
inferMain program = inferBinding program main
  where
    Program main _ = program

inferBinding :: Program Type -> Binding Type -> [MType] -> MType
inferBinding program binding argTypes =
  runReader (fromInfer $ inferBinding' binding argTypes)
           $ emptyEnvironment program

type Stack       = [Application]
type Application = (Identifier, [MType])

data Environment = Environment {
    envVarBindings :: M.Map Identifier MType
  , envStack       :: Stack
  , envProgram     :: Program Type
}
emptyEnvironment = Environment M.empty []

newtype Infer a = Infer { fromInfer :: Reader Environment a }
                deriving (Monad, MonadReader Environment)

inferBinding' :: Binding Type -> [MType] -> Infer MType
inferBinding' (Binding b params exp) argTypes = 
  assert (length params == length argTypes) $ do
    program <- asks envProgram

    asks (elem (identifier b, argTypes) . envStack) >>= \case
      True  -> return $ makeKnown program $ resultType $ idType b
      False -> local updateEnv $ inferExpression exp

  where
    updateEnv env = 
      env { envVarBindings = foldl 
              (\bindings (param,type_) -> M.insert (identifier param) type_ bindings)
              (envVarBindings env) 
              (zip params argTypes)
          , envStack = (identifier b, argTypes) : (envStack env)
          }

inferExpression :: Expression Type -> Infer MType
inferExpression = \case 
  ExpVar v -> asks $ fromJust . M.lookup (identifier v) . envVarBindings

  ExpCon (TypedIdentifier _ t) -> do
    program <- asks envProgram
    return $ makeKnown program t

  ExpApp (ExpVar v) args -> do
    argsTypes <- forM args inferExpression
    binding   <- asks $ bindingFromName v . envProgram
    inferBinding' binding argsTypes 

  ExpApp (ExpCon c) args -> do
    argsTypes <- forM args inferExpression
    asks $ \env -> inferConstructorApp (envProgram env) c argsTypes 

  ExpCase d branches -> do
    dType       <- inferExpression d
    branchTypes <- forM branches $ inferBranch dType

    if topmost dType == Unknown
      then return $ toMaxUnknown $ head branchTypes
      else return $ supremumMType branchTypes

  ExpLet (Binding name [] value) exp -> do
    valueType <- inferExpression value
    local (updateEnv valueType) $ inferExpression exp
    where
      updateEnv valueType env =
        env { envVarBindings = M.insert (identifier name) valueType $ envVarBindings env }

inferConstructorApp :: Program Type -> TypedIdentifier Type -> [MType] -> MType
inferConstructorApp program cId cArgTypes =
  (resultType $ idType cId) { typeAnnotation = supremum appliedArgModes }
  where 
    adt         = adtFromConstructorName cId program
    constructor = constructorFromName    cId program

    appliedArgModes = zipWith apply [0..] $ map typeAnnotation cArgTypes
      where
        apply i argMode = replaceSubMode adt constructor i knownResultMode argMode

        knownResultMode = typeAnnotation $ makeKnown program $ resultType $ idType cId

inferBranch :: MType -> Branch Type -> Infer MType
inferBranch dType (Branch pat exp) = do
  program <- asks envProgram
  local (updateEnv $ inferNewVarBindings program) $ inferExpression exp
  where
    inferNewVarBindings program = case inferPattern program dType pat of
      PatVar v    -> [(identifier v, idType v)]
      PatCon _ vs -> map (\(TypedIdentifier v t) -> (v,t)) vs

    updateEnv newVarBindings env = 
      env { envVarBindings = M.union (M.fromList newVarBindings) $ envVarBindings env }

inferPattern :: Program Type -> MType -> Pattern Type -> Pattern MType
inferPattern program dType pattern = case pattern of
  PatVar v    -> PatVar $ TypedIdentifier (identifier v) dType
  PatCon c vs -> PatCon c $ zipWith TypedIdentifier (map identifier vs) mtypes
    where
      cId           = TypedIdentifier c undefined
      adt           = adtFromConstructorName cId $ program
      constructor   = constructorFromName    cId $ program
      mtypes        = zipWith getType vs [0..]
      getType var i =
        (idType var) { typeAnnotation = submode adt constructor i $ typeAnnotation dType }
