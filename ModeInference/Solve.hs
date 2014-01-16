module ModeInference.Solve
where

import           Control.Exception (assert)
import           Data.Generics
import qualified Data.Map as M
import           ModeInference.Language
import           ModeInference.Constraint
import           ModeInference.Inference (ModeInstanceNames)

type Assignment = M.Map Identifier Mode

getMode :: Identifier -> Assignment -> Mode
getMode id sigma = case M.lookup id sigma of
  Nothing   -> error $ "ModeInference.Solve.getMode: unassigned mode variable '" ++ id ++ "'"
  Just mode -> mode

solve :: [IModeConstraint] -> Assignment
solve constraints = runSolver constraints M.empty

runSolver :: [IModeConstraint] -> Assignment -> Assignment
runSolver []     sigma = sigma
runSolver (c:cs) sigma = case c of
  IModeEq (IMVar v) (Mode m) -> assert (not $ v `elem` M.keys sigma) $
    runSolver cs' $ M.insert v m sigma
    where
      cs' = replace v m cs

  IModeMax (IMVar v) ms -> assert (not $ v `elem` M.keys sigma) $
    runSolver cs' $ M.insert v m sigma
    where
      m   = reduceMaximum ms
      cs' = replace v m cs

replace :: (Typeable a, Data a) => Identifier -> Mode -> a -> a
replace id mode' = everywhere $ mkT go
  where
    go (IMVar v) | v == id = Mode mode'
    go mode                = mode

reduceMaximum :: [IMode] -> Mode
reduceMaximum ms = if Mode Unknown `elem` ms
                   then Unknown
                   else Known

reconstructMProgram :: Assignment -> Program IMType -> ModeInstanceNames -> Program MType
reconstructMProgram sigma program instances = 
  collapseModeInstances sigma instances $ assignModeVariables sigma program

assignModeVariables :: (Functor a) => Assignment -> a IMType -> a MType
assignModeVariables sigma = fmap mapType
  where 
    mapType t = t { typeAnnotation = mapMode     $ typeAnnotation t 
                  , typeArguments  = map mapType $ typeArguments  t
                  }
    mapMode (Mode  m) = m
    mapMode (IMVar v) = getMode v sigma

collapseModeInstances :: Assignment -> ModeInstanceNames -> Program MType -> Program MType
collapseModeInstances sigma instances program = program

  where
    instances' = map (\((i,a),v) -> ((i,assignModeVariables sigma a),v))
               $ M.toList instances

    {-
    duplicates []  = []
    duplicates [a] = []
    duplicates (k
    -}
