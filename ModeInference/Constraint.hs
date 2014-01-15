{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
module ModeInference.Constraint
where

import Control.Exception (assert)
import Data.List (partition,transpose,nub)
import Data.Generics (Data,Typeable)
import Data.Maybe (catMaybes)
import ModeInference.Language
import ModeInference.Type

data IMTypeConstraint = IMTypeEq   IMType IMType
                      | IMTypeSup  IMType [IMType]
                      | IMTypeCase IMType IMType [IMType]
                      deriving (Show,Eq,Data,Typeable)

data IModeConstraint = IModeEq   IMode IMode
                     | IModeMax  IMode [IMode]
                     deriving (Show,Eq,Data,Typeable)

modeConstraints :: [IMTypeConstraint] -> [IModeConstraint]
modeConstraints = collapseMax . concatMap go
  where
    go (IMTypeEq t1 t2)                        = catMaybes $ goEq t1 t2
    go (IMTypeCase e (AnnotatedType _ m _) bs) = goCase m e bs
    go (IMTypeSup e bs)                        = catMaybes $ goSup e bs

    goEq (AnnotatedType id1 m1 ts1) (AnnotatedType id2 m2 ts2) = 
        assert (id1 == id2)
      $ assert (length ts1 == length ts2)
      $ ( if m1 == m2 then Nothing else Just (IModeEq m1 m2) ) 
      : ( concat $ zipWith goEq ts1 ts2 )
      
    goSup (AnnotatedType _ eM eTs) bs = 
        (Just $ IModeMax eM $ map typeAnnotation bs)
      : (goSups eTs $ map typeArguments bs)

    goSups :: [IMType] -> [[IMType]] -> [Maybe IModeConstraint]
    goSups eTs subTss = 
        assert (all (\ts -> length ts == length (head subTss)) subTss)
      $ concat 
      $ zipWith goSup eTs subTss'
      where
        subTss' = transpose subTss

    goCase dM (AnnotatedType _ eM eTs) bs =
        IModeMax eM (dM : (map typeAnnotation bs))
      : (goCases dM eTs $ map typeArguments bs)

    goCases :: IMode -> [IMType] -> [[IMType]] -> [IModeConstraint]
    goCases dM eTs subTss = 
        assert (all (\ts -> length ts == length (head subTss)) subTss)
      $ concat 
      $ zipWith (goCase dM) eTs subTss'
      where
        subTss' = transpose subTss

    collapseMax constraints = case constraints of
      [] -> []
      (IModeMax v ms):cs -> (IModeMax v $ nub $ ms ++ ms') : (collapseMax rest)
        where 
          (maxV,rest) = partition (\case {IModeMax v' _ -> v' == v; _ -> False}) cs
          ms'         = concatMap (\(IModeMax _ ms') -> ms') maxV

      c:cs -> c : (collapseMax cs)

mainArgumentConstraints :: Program IMType -> [MType] -> [IMTypeConstraint]
mainArgumentConstraints (Program main _) argTypes =
    assert (length argTypes == length paramTypes)
  $ zipWith (\p a -> IMTypeEq p (iMTypeFromMType a)) paramTypes argTypes 
  where
    paramTypes = map idType $ bindParameters main
