{-# LANGUAGE LambdaCase #-}
module ModeInference.Constraint.Solve
  (Assignment, unionAssignment, solveDeterministic, solveNonDeterministic)
where

import           Control.Monad (guard)
import           Data.Generics
import qualified Data.Map as M
import           Data.List (nub)
import           ModeInference.Language hiding (Binding)
import           ModeInference.Constraint

type Assignment = M.Map Identifier Mode
type Binding    = (Identifier, Mode)

unionAssignment :: Assignment -> Assignment -> Assignment
unionAssignment a b = M.map ground union
  where
    union = M.unionWith (error "Constraint.Solve.unionAssignment") a b

    ground Unknown     = Unknown
    ground Known       = Known
    ground (ModeVar v) = case M.lookup v union of
                            Nothing -> ModeVar v
                            Just v' -> ground v'

data Predicate = IsSatisfied
               | IsUnsatisfiable
               | DontKnow
               deriving (Eq)

data Propagate1 = Bind [Binding]
                | Unsatisfiable1
                | Remove
                | Skip

data PropagateN = BindIn [Binding] [ModeConstraint]
                | UnsatisfiableN
                | NothingToPropagate

solveDeterministic :: [ModeConstraint] -> (Assignment, [ModeConstraint])
solveDeterministic constraints = runDeterministicSolver constraints M.empty

runDeterministicSolver :: [ModeConstraint] -> Assignment -> (Assignment, [ModeConstraint])
runDeterministicSolver []          sigma = (sigma, [])
runDeterministicSolver constraints sigma = case propagateN constraints of
  NothingToPropagate -> (sigma, constraints)
  UnsatisfiableN     -> error "Constraint.Solve.runDeterministicSolver: no solution"

  BindIn bindings constraints' -> runDeterministicSolver constraints'' sigma'
    where
      constraints'' = replace bindings constraints'
      sigma'        = M.fromList bindings `M.union` sigma

propagateN :: [ModeConstraint] -> PropagateN
propagateN []     = NothingToPropagate
propagateN (c:cs) = case propagate1 c of
  Bind bs        -> BindIn bs cs
  Unsatisfiable1 -> UnsatisfiableN
  Remove         -> propagateN cs
  Skip           -> case propagateN cs of BindIn bs cs' -> BindIn bs (c:cs')
                                          p             -> p

propagate1 :: ModeConstraint -> Propagate1
propagate1 constraint = case constraint of
  ModeImpl ps cs ->
    if      all (IsSatisfied     ==) predicates then foldl toBinding (Bind []) cs
    else if any (IsUnsatisfiable ==) predicates then Remove
    else Skip
    where 
      predicates = map predicate ps

      predicate (Unknown, Unknown) = IsSatisfied
      predicate (Known  , Known  ) = IsSatisfied
      predicate (Known  , Unknown) = IsUnsatisfiable
      predicate (Unknown, Known  ) = IsUnsatisfiable
      predicate _                  = DontKnow

      toBinding Unsatisfiable1 _             = Unsatisfiable1
      toBinding (Bind bs) (Unknown, Unknown) = Bind bs
      toBinding (Bind bs) (Known  , Known  ) = Bind bs
      toBinding (Bind _ ) (Unknown, Known  ) = Unsatisfiable1
      toBinding (Bind _ ) (Known  , Unknown) = Unsatisfiable1

      toBinding (Bind bs) (ModeVar v, m)     = Bind $ (v, m) : bs
      toBinding (Bind bs) (m, ModeVar v)     = Bind $ (v, m) : bs

  ModeLT Known       Known       -> Remove
  ModeLT Unknown     Unknown     -> Remove
  ModeLT Known       Unknown     -> Remove
  ModeLT Unknown     Known       -> Unsatisfiable1
  ModeLT Unknown     (ModeVar v) -> Bind [(v, Unknown)]
  ModeLT (ModeVar _) Unknown     -> Skip
  ModeLT (ModeVar v) Known       -> Bind [(v, Known)]
  ModeLT Known       (ModeVar _) -> Skip
  ModeLT (ModeVar _) (ModeVar _) -> Skip

solveNonDeterministic :: [ModeConstraint] -> [Assignment]
solveNonDeterministic constraints = allAssignments
  where
    unassignedIds = nub $ everything (++) (mkQ [] $ \case ModeVar v -> [v]
                                                          _         -> [ ]) constraints
    allAssignments = do
      values <- sequence $ replicate (length unassignedIds) [Known, Unknown]
      let bindings = zip unassignedIds values
          sigma    = M.fromList bindings
          
      guard $ runNonDeterministicSolver (replace bindings constraints) sigma

      return sigma

runNonDeterministicSolver :: [ModeConstraint] -> Assignment -> Bool
runNonDeterministicSolver []          _     = True
runNonDeterministicSolver constraints sigma = case propagateN constraints of
  NothingToPropagate -> True
  UnsatisfiableN     -> False
  BindIn [] cs       -> runNonDeterministicSolver cs sigma
  BindIn _  _        -> error "Constraint.Solve.solveNonDeterministic"

replace :: (Typeable a, Data a) => [Binding] -> a -> a
replace bs a = foldl (flip replace') a bs
  where
    replace' (id,mode') = everywhere $ mkT go
      where
        go (ModeVar v) | v == id = mode'
        go mode                  = mode
