{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
module ModeInference.Constraint
where

import Control.Exception (assert)
import Data.List (partition,transpose,nub)
import Data.Generics (Data,Typeable)
import Data.Maybe (catMaybes)
import ModeInference.Language

data ModeConstraint = ModeEq   Mode Mode
                    | ModeSup  Mode [Mode]
                    | ModeCase Mode Mode [Mode]
                    deriving (Show,Eq,Data,Typeable)

data ModeAtomConstraint = ModeAtomEq   ModeAtom ModeAtom
                        | ModeAtomMax  ModeAtom [ModeAtom]
                        deriving (Show,Eq,Data,Typeable)

modeAtomConstraints :: [ModeConstraint] -> [ModeAtomConstraint]
modeAtomConstraints = collapseMax . concatMap go
  where
    go (ModeEq m1 m2)    = catMaybes $ goEq m1 m2
    go (ModeCase e d bs) = goCase (topmostMode d) e bs
    go (ModeSup e bs)    = goSup e bs

    goEq (Mode a1 mss1) (Mode a2 mss2) = c:cs
      where
        c  = if a1 == a2 then Nothing else Just (ModeAtomEq a1 a2)
        cs = goSubmodes mss1 [mss2] 
           $ \ms1 ms2 -> goSubmodes ms1 ms2 
           $ \m1 [m2] -> goEq m1 m2

    goSup m bs = c:cs
      where 
        c  = ModeAtomMax (topmostMode m) $ map topmostMode bs
        cs = goSubmodes (submodes m) (map submodes bs)
           $ \ms bss -> goSubmodes ms bss goSup

    goCase d e bs = c:cs
      where
        c  = ModeAtomMax (topmostMode e) (d : (map topmostMode bs))
        cs = goSubmodes (submodes e) (map submodes bs)
           $ \es bss -> goSubmodes es bss 
           $ goCase d

    goSubmodes :: [a] -> [[a]] -> (a -> [a] -> [b]) -> [b]
    goSubmodes as bss f = 
        assert (all (\bs -> length bs == length as) bss)
      $ concat 
      $ zipWith f as
      $ transpose bss

    collapseMax constraints = case constraints of
      [] -> []
      (ModeAtomMax v ms):cs -> (ModeAtomMax v $ nub $ ms ++ ms') : (collapseMax rest)
        where 
          (maxV,rest) = partition (\case {ModeAtomMax v' _ -> v' == v; _ -> False}) cs
          ms'         = concatMap (\(ModeAtomMax _ ms') -> ms') maxV

      c:cs -> c : (collapseMax cs)

mainArgumentConstraints :: Program MType -> [Mode] -> [ModeConstraint]
mainArgumentConstraints (Program main _) argModes =
    assert (length argModes == length paramModes)
  $ zipWith ModeEq paramModes argModes
  where
    paramModes = map (typeAnnotation . idType) $ bindParameters main
