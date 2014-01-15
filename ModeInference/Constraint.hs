{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
module ModeInference.Constraint
where

import Control.Exception (assert)
import Data.List (partition,transpose,nub)
import Data.Generics (Data,Typeable)
import Data.Maybe (catMaybes)
import ModeInference.Language
import ModeInference.Util

data MTypeConstraint = MTypeEq   MType MType
                     | MTypeSup  MType [MType]
                     | MTypeCase MType MType [MType]
                     deriving (Show,Eq,Data,Typeable)

data ModeConstraint = ModeEq   Mode Mode
                    | ModeMax  Mode [Mode]
                    deriving (Show,Eq,Data,Typeable)

modeConstraints :: [MTypeConstraint] -> [ModeConstraint]
modeConstraints = collapseMax . concatMap go
  where
    go (MTypeEq t1 t2)                = catMaybes $ goEq t1 t2
    go (MTypeCase e (MType _ m _) bs) = goCase m e bs
    go (MTypeSup e bs)                = catMaybes $ goSup e bs

    goEq (MType id1 m1 ts1) (MType id2 m2 ts2) = 
        assert (id1 == id2)
      $ assert (length ts1 == length ts2)
      $ ( if m1 == m2 then Nothing else Just (ModeEq m1 m2) ) 
      : ( concat $ zipWith goEq ts1 ts2 )
      
    goSup (MType _ eM eTs) bs = 
        (Just $ ModeMax eM $ map modeOf bs)
      : (goSups eTs $ map subMTypes bs)

    goSups :: [MType] -> [[MType]] -> [Maybe ModeConstraint]
    goSups eTs subTss = 
        assert (all (\ts -> length ts == length (head subTss)) subTss)
      $ concat 
      $ zipWith goSup eTs subTss'
      where
        subTss' = transpose subTss

    goCase dM (MType _ eM eTs) bs =
        ModeMax eM (dM : (map modeOf bs))
      : (goCases dM eTs $ map subMTypes bs)

    goCases :: Mode -> [MType] -> [[MType]] -> [ModeConstraint]
    goCases dM eTs subTss = 
        assert (all (\ts -> length ts == length (head subTss)) subTss)
      $ concat 
      $ zipWith (goCase dM) eTs subTss'
      where
        subTss' = transpose subTss

    collapseMax constraints = case constraints of
      [] -> []
      (ModeMax v ms):cs -> (ModeMax v $ nub $ ms ++ ms') : (collapseMax rest)
        where 
          (maxV,rest) = partition (\case {ModeMax v' _ -> v' == v; _ -> False}) cs
          ms'         = concatMap (\(ModeMax _ ms') -> ms') maxV

      c:cs -> c : (collapseMax cs)

mainArgumentConstraints :: Program MType -> [MType] -> [MTypeConstraint]
mainArgumentConstraints (Program main _) argMTypes =
    assert (length argMTypes == length paramMTypes)
  $ zipWith MTypeEq paramMTypes argMTypes 
  where
    paramMTypes = map annIdAnnotation $ bindParameters main
