{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
module ModeInference.Inference
  (inferMain, inferBinding, inferConstructorApp)
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
  assert (length params == length argTypes) $

  asks (elem (identifier b, argTypes) . envStack) >>= \case
    True  -> return $ makeKnown $ resultType $ idType b
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

  ExpCon (TypedIdentifier _ (AnnotatedType t () [])) ->
    return $ AnnotatedType t Known []

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
      else return $ supremum branchTypes

  ExpLet (Binding name [] value) exp -> do
    valueType <- inferExpression value
    local (updateEnv valueType) $ inferExpression exp
    where
      updateEnv valueType env =
        env { envVarBindings = M.insert (identifier name) valueType $ envVarBindings env }

inferConstructorApp :: Program Type -> TypedIdentifier Type -> [MType] -> MType
inferConstructorApp program cId cArgTypes = supremum appliedArgTypes
  where 
    adt         = adtFromConstructorName cId program
    constructor = constructorFromName    cId program

    appliedArgTypes = zipWith apply [0..] cArgTypes
      where
        apply i argType = case adtVarIndexByConstructorArgIndex adt constructor i of
          Nothing -> argType
          Just i  -> replaceTypeArgument i argType knownResultType 

        knownResultType = makeKnown $ resultType $ idType cId

inferBranch :: MType -> Branch Type -> Infer MType
inferBranch dType (Branch pat exp) = do
  newVarBindings <- inferNewVarBindings
  local (updateEnv newVarBindings) $ inferExpression exp
  where
    inferNewVarBindings = case pat of
      PatVar v    -> return [(identifier v, dType)]
      PatCon c vs -> do
        adt         <- asks $ adtFromConstructorName (TypedIdentifier c undefined) . envProgram
        constructor <- asks $ constructorFromName    (TypedIdentifier c undefined) . envProgram

        let ids    = map identifier vs
            mtypes = map (getType adt constructor) [0..]

        return $ zip ids mtypes
        where
          getType adt constructor i =
            case adtVarIndexByConstructorArgIndex adt constructor i of
              Nothing -> dType
              Just n  -> nthSubtype n dType

    updateEnv newVarBindings env = 
      env { envVarBindings = M.union (M.fromList newVarBindings) $ envVarBindings env }

makeKnown :: Type -> MType
makeKnown (AnnotatedType id _ ts) = AnnotatedType id Known $ map makeKnown ts

replaceTypeArgument :: Int -> AnnotatedType a -> AnnotatedType a -> AnnotatedType a
replaceTypeArgument n newArg type_ = 
  type_ { typeArguments = go n $ typeArguments type_ }
  where
    go _ []     = error "Inference.replaceTypeArgument"
    go 0 (_:ts) = newArg:ts
    go i (t:ts) = t : (go (i-1) ts)
