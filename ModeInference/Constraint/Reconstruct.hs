{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
module ModeInference.Constraint.Reconstruct
  (minimalProgram)
where

import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Data.Generics
import qualified Data.Map as M
import qualified Data.Set as S
import           ModeInference.Language
import           ModeInference.Util (removeBinding,bindingFromName)
import           ModeInference.Constraint.Inference (ModeInstanceNames)
import           ModeInference.Type
import           ModeInference.Constraint.Solve (Assignment)

minimalProgram :: Program MType -> ModeInstanceNames -> Assignment -> Program MType
minimalProgram program instanceNames sigma = program'
  where
    program' = removeUnreachableModeInstances instanceNames
             $ assignModeInstances instanceNames 
             $ assignModeVariables sigma program

type Reached = S.Set (TypedIdentifier MType)
type Env     = Program MType

newtype Reachable a = Reachable { fromReachable :: ReaderT Env (State Reached) a }
  deriving (Monad, MonadReader Env, MonadState Reached)

removeUnreachableModeInstances :: ModeInstanceNames -> Program MType -> Program MType
removeUnreachableModeInstances instanceNames p = 
  foldl (flip removeBinding) p $ S.toList unreachable
  where
    reachable   = execState (runReaderT (fromReachable $ reachableProgram p) p) S.empty
    unreachable = (S.fromList $ M.elems instanceNames)
                  `S.difference` 
                  (S.map identifier reachable)

reachableProgram (Program main _) = reachableBinding main
reachableBinding                  = reachableExp . bindExpression
reachableExp expression = case expression of
  ExpVar {}               -> return ()
  ExpCon {}               -> return ()
  ExpApp (ExpCon {}) args -> forM_ args reachableExp
  ExpApp (ExpVar v ) args -> do
    forM_ args reachableExp
    gets (S.member v) >>= \case True  -> return ()
                                False -> do modify (S.insert v)
                                            asks (bindingFromName v) >>= reachableBinding
  ExpCase d bs -> do
    reachableExp d
    forM_ bs $ \(Branch _ b) -> reachableExp b

  ExpLet b e -> reachableBinding b >> reachableExp e

assignModeVariables :: (Typeable a, Data a) => Assignment -> a -> a
assignModeVariables sigma = everywhere $ mkT go
  where
    go (ModeVar v) = case M.lookup v sigma of
      Nothing   -> error $ "Constraint.Reconstruct.assignModeVariables: '" ++ v ++ "' not found"
      Just atom -> atom

    go mode = mode

assignModeInstances :: (Typeable a, Data a) => ModeInstanceNames -> a -> a
assignModeInstances instanceNames = everywhere $ mkT go
  where
    go (ExpApp (ExpVar v) args) = 
      case M.lookup key instanceNames of
        Nothing -> error $ "Constraint.Reconstruct.assignModeInstances: '" ++ (show key) ++ "' not found"
        Just i  -> ExpApp (ExpVar $ v { identifier = i }) args
      where
        key = (identifier v, argumentMTypes $ idType v)

    go exp = exp
