{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module ModeInference.Constraint.Solve
  (Assignment, dumpCNF, solve)
where

import           Prelude hiding (not,and)
import           Control.Monad.State.Strict hiding (State)
import qualified Data.Map as M
import           Satchmo.Core.MonadSAT
import           Satchmo.Core.Primitive
import           Satchmo.Core.Boolean (Boolean)
import qualified Satchmo.Core.SAT.Minisat as Minisat
import qualified Satchmo.Core.SAT.StdOut as StdOut
import           Satchmo.Core.Decode
import           ModeInference.Language hiding (Binding)
import           ModeInference.Constraint

type Assignment = M.Map Identifier Mode

dumpCNF :: [ModeConstraint] -> IO ()
dumpCNF constraints = 
  void $ StdOut.onStdOut 
       $ execStateT (fromEmit ((emitConstraints constraints) :: Emit StdOut.SAT Boolean ())) M.empty

solve :: [ModeConstraint] -> IO (Maybe Assignment)
solve constraints = 
  Minisat.solve $ do
    s <- execStateT (fromEmit ((emitConstraints constraints) :: Emit Minisat.SAT Boolean ())) M.empty
    return $ decode s
  
type State p = M.Map Identifier p

instance Decode Minisat.SAT Boolean Mode where
  decode b = decode b >>= \case
    False -> return Known
    True  -> return Unknown

newtype Emit m p a = Emit { fromEmit :: StateT (State p) m a }
                     deriving (Monad, MonadSAT, MonadState (State p))
  
emitConstraints :: (MonadSAT m, Primitive p) => [ModeConstraint] -> Emit m p ()
emitConstraints = mapM_ go
  where
    go (ModeMax m ms) = do
      b  <- emitMode m
      bs <- mapM emitMode ms
      case bs of
        []  -> do assert [not b]
        [x] -> do assert [not x, b]
                  assert [not b, x]
        _   -> do sequence_ $ do b' <- bs
                                 return $ assert [not b', b]
                  assert $ not b : bs

    go (ModeImpl ps c) = do
      ps' <- mapM equalModes ps >>= and
      c'  <- equalModes c
      r   <- ps' `implies` c'
      assert [r]
      where
        equalModes (m1, m2) = do
          m1' <- emitMode m1
          m2' <- emitMode m2
          equals [m1', m2']

emitMode :: (MonadSAT m, Primitive p) => Mode -> Emit m p p
emitMode Known   = return $ constant False
emitMode Unknown = return $ constant True
emitMode (ModeVar v) = 
  gets (M.lookup v) >>= \case
    Just b  -> return b
    Nothing -> do
      b <- primitive
      --note $ v ++ " -> " ++ (show b)
      modify $ M.insert v b
      return b
