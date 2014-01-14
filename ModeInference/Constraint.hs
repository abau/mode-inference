{-# LANGUAGE DeriveDataTypeable #-}
module ModeInference.Constraint
where

import Control.Exception (assert)
import Data.List (transpose)
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
                    | ModeCase Mode Mode [Mode]
                    deriving (Show,Eq,Data,Typeable)

modeConstraints :: [MTypeConstraint] -> [ModeConstraint]
modeConstraints = concatMap go
  where
    go (MTypeEq t1 t2)                = catMaybes $ goEq t1 t2
    go (MTypeCase e (MType _ m _) bs) = goCase m e bs
    go (MTypeSup e bs)                = catMaybes $ goSup e bs

    goEq (MType id1 m1 ts1) (MType id2 m2 ts2) = 
        assert (id1 == id2)
      $ assert (length ts1 == length ts2)
      $ ( if m1 == m2 then Nothing else Just (ModeEq m1 m2) ) 
      : ( concat $ zipWith goEq ts1 ts2 )
      
    goSup _ []                = error "Constraint.modeConstraints.goSup"
    goSup e [b]               = goEq e b
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
        ModeCase eM dM (map modeOf bs) 
      : (goCases dM eTs $ map subMTypes bs)

    goCases :: Mode -> [MType] -> [[MType]] -> [ModeConstraint]
    goCases dM eTs subTss = 
        assert (all (\ts -> length ts == length (head subTss)) subTss)
      $ concat 
      $ zipWith (goCase dM) eTs subTss'
      where
        subTss' = transpose subTss

mainArgumentConstraints :: Program MType -> [MType] -> [MTypeConstraint]
mainArgumentConstraints (Program main _) argMTypes =
    assert (length argMTypes == length paramMTypes)
  $ zipWith MTypeEq paramMTypes argMTypes 
  where
    paramMTypes = map annIdAnnotation $ bindParameters main
