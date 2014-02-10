{-# LANGUAGE LambdaCase #-}
module ModeInference.Constraint.Solve
  (Assignment, propagate)
where

import           Control.Monad (guard)
import           Data.Generics
import qualified Data.Map as M
import           ModeInference.Language hiding (Binding)
import           ModeInference.Constraint

type Assignment = M.Map Identifier Mode
type Binding    = (Identifier, Mode)

data Predicate = IsSatisfied
               | IsUnsatisfiable
               | DontKnow
               deriving (Eq)

data Propagate1 = Bind Binding
                | Unsatisfiable1
                | Remove
                | Skip ModeConstraint

data PropagateN = BindIn Binding [ModeConstraint]
                | UnsatisfiableN
                | NothingToPropagate [ModeConstraint]

propagate :: [ModeConstraint] -> (Assignment, [ModeConstraint])
propagate constraints = runPropagationSolver constraints M.empty

runPropagationSolver :: [ModeConstraint] -> Assignment -> (Assignment, [ModeConstraint])
runPropagationSolver []          sigma = (sigma, [])
runPropagationSolver constraints sigma = case propagateN constraints of
  NothingToPropagate constraints' -> 
    case resolution constraints' of
      Nothing           -> (sigma, constraints')
      Just constaints'' -> runPropagationSolver constaints'' sigma

  UnsatisfiableN -> error "Constraint.Solve.runPropagationSolver: no solution"

  BindIn (k,v) constraints' -> runPropagationSolver constraints'' sigma'
    where
      constraints'' = replace (k,v) constraints'
      sigma'        = M.insert k v sigma

propagateN :: [ModeConstraint] -> PropagateN
propagateN []     = NothingToPropagate []
propagateN (c:cs) = case propagate1 c of
  Bind b         -> BindIn b cs
  Unsatisfiable1 -> UnsatisfiableN
  Remove         -> propagateN cs
  Skip c'        -> case propagateN cs of 
    NothingToPropagate cs' -> NothingToPropagate $ c':cs'
    UnsatisfiableN         -> UnsatisfiableN
    BindIn b cs'           -> BindIn b (c':cs')

propagate1 :: ModeConstraint -> Propagate1
propagate1 constraint@(ModeImpl ps c) = case (ps,c) of
  (_ , (Known    , _        )) -> Remove
  (_ , (_        , Unknown  )) -> Remove

  ([], (Unknown  , Known    )) -> Unsatisfiable1
  ([], (Unknown  , ModeVar v)) -> Bind (v, Unknown)
  ([], (ModeVar v, Known    )) -> Bind (v, Known)
  ([], (ModeVar _, ModeVar _)) -> Skip constraint

  _ -> if any (== IsUnsatisfiable) (map predicate ps) then Remove
       else if (length ps == length ps') then Skip constraint
            else propagate1 $ ModeImpl ps' c
  where
    ps' = filter (\p -> predicate p == DontKnow) ps

    predicate (Known  , _      ) = IsSatisfied
    predicate (_      , Unknown) = IsSatisfied
    predicate (Unknown, Known  ) = IsUnsatisfiable
    predicate _                  = DontKnow

resolution :: [ModeConstraint] -> Maybe [ModeConstraint]
resolution constraints = case resolutionCandidate constraints of
  Nothing                         -> Nothing
  Just (constraint1, constraint2) -> Just $ cs ++ constraints'
    where
      ModeImpl _ c1 = constraint1
      ModeImpl _ c2 = constraint2
      cs           = if c1 == c2 then [ ModeImpl [] c1] 
                                 else [ ModeImpl [] c1
                                      , ModeImpl [] c2]
      constraints' = filter (/= constraint1)
                   $ filter (/= constraint2) constraints

resolutionCandidate :: [ModeConstraint] -> Maybe (ModeConstraint,ModeConstraint)
resolutionCandidate constraints = case result of
  []         -> Nothing
  (Just c):_ -> Just c
  where
    result = do x1@(ModeImpl ps1 c1) <- constraints
                x2@(ModeImpl ps2 c2) <- constraints
                guard $ c1 == c2
                guard $ isCompatible ps1 ps2
                return $ Just (x1,x2)

    isCompatible [(Unknown, ModeVar v1)] [(ModeVar v2, Known)] = v1 == v2
    isCompatible [(ModeVar v2, Known)] [(Unknown, ModeVar v1)] = v2 == v1
    isCompatible _ _                                           = False

replace :: (Typeable a, Data a) => Binding -> a -> a
replace (id,mode') = everywhere $ mkT go
  where
    go (ModeVar v) | v == id = mode'
    go mode                  = mode
