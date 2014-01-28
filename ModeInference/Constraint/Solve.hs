module ModeInference.Constraint.Solve
  (Assignment, solveDeterministic, assignModeVariables, assignModeInstances)
where

import           Data.Generics
import           Data.Maybe (mapMaybe)
import qualified Data.Map as M
import           ModeInference.Language hiding (Binding)
import           ModeInference.Constraint
import           ModeInference.Constraint.Inference (ModeInstanceNames)
import           ModeInference.Util
import           ModeInference.Type

type Assignment = M.Map Identifier ModeAtom
type Binding    = (Identifier, ModeAtom)

data Predicate = Satisfied
               | Unsatisfiable
               | DontKnow
               deriving (Eq)

solveDeterministic :: [ModeAtomConstraint] -> (Assignment, [ModeAtomConstraint])
solveDeterministic constraints = runDeterministicSolver constraints M.empty

runDeterministicSolver :: [ModeAtomConstraint] -> Assignment -> (Assignment, [ModeAtomConstraint])
runDeterministicSolver []          sigma = (sigma, [])
runDeterministicSolver constraints sigma = case hasDeterministicRule constraints of
  Nothing                     -> (sigma, constraints)
  Just (bindings, constraint) -> runDeterministicSolver constraints' sigma'
    where
      constraints' = replace' bindings $ filter (/= constraint) constraints
      sigma'       = M.fromList bindings `M.union` sigma

hasDeterministicRule :: [ModeAtomConstraint] -> Maybe ([Binding], ModeAtomConstraint)
hasDeterministicRule []     = Nothing
hasDeterministicRule (c:cs) = case isDeterministicRule c of
  Nothing -> hasDeterministicRule cs
  Just s  -> Just (s, c)

isDeterministicRule :: ModeAtomConstraint -> Maybe [Binding]
isDeterministicRule constraint = case constraint of
  ModeAtomImpl ps cs ->
    if      all (Satisfied     ==) predicates then Just $ mapMaybe toBinding cs
    else if any (Unsatisfiable ==) predicates then Just [] -- just removes constraint
    else Nothing
    where 
      predicates = map predicate ps

      predicate (Unknown, Unknown) = Satisfied
      predicate (Known  , Known  ) = Satisfied
      predicate (Known  , Unknown) = Unsatisfiable
      predicate (Unknown, Known  ) = Unsatisfiable
      predicate _                  = DontKnow

      toBinding (Unknown, Unknown) = Nothing
      toBinding (Known  , Known  ) = Nothing
      toBinding (Unknown, Known  ) = error "Constraint.Solve.isDeterministicRule: no solution"
      toBinding (Known  , Unknown) = error "Constraint.Solve.isDeterministicRule: no solution"

      toBinding (ModeVar v, m) = Just (v, m)
      toBinding (m, ModeVar v) = Just (v, m)


  ModeAtomMax m ms -> case isConstantAtom m of
    True -> error "Constraint.Solve.isDeterministicRule: constant max"

    False | length ms == 1    -> Just [(mVar, head ms)]
    False | Unknown `elem` ms -> Just [(mVar, Unknown)]
    False                     -> Nothing
    where
      ModeVar mVar = m

replace :: (Typeable a, Data a) => Binding -> a -> a
replace (id,mode') = everywhere $ mkT go
  where
    go (ModeVar v) | v == id = mode'
    go mode                  = mode

replace' :: (Typeable a, Data a) => [Binding] -> a -> a
replace' bs a = foldl (flip replace) a bs

assignModeVariables :: (Typeable a, Data a) => Assignment -> a -> a
assignModeVariables sigma = everywhere $ mkT go
  where
    go (ModeVar v) = case M.lookup v sigma of
      Nothing   -> ModeVar v
      Just atom -> atom

    go mode = mode

assignModeInstances :: (Typeable a, Data a) => ModeInstanceNames -> a -> a
assignModeInstances instanceNames = everywhere $ mkT go
  where
    go (ExpApp (ExpVar v) as) = case M.lookup key instanceNames of
      Nothing -> ExpApp (ExpVar v) as
      Just i  -> ExpApp (ExpVar $ v { identifier = i }) as
      
      where
        key = (identifier v, map typeAnnotation $ argumentTypes $ idType v)

    go exp = exp
