{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
module ModeInference.Constraint
where

import Control.Exception (assert)
import Data.List (partition,transpose,nub)
import Data.Generics (Data,Typeable)
import ModeInference.Language

data ModeConstraint = ModeImpl [(Mode,Mode)] (Mode,Mode)
                    | ModeSup  Mode [Mode]
                    | ModeCase Mode Mode [Mode]
                    deriving (Show,Eq,Data,Typeable)

modeImpl :: [(MType,MType)] -> (MType,MType) -> ModeConstraint
modeImpl ps c = ModeImpl (map go ps) (go c)
  where
    go (x,y) = (typeAnnotation x, typeAnnotation y)

modeSup :: MType -> [MType] -> ModeConstraint
modeSup a bs = ModeSup (typeAnnotation a) (map typeAnnotation bs)

modeCase :: MType -> MType -> [MType] -> ModeConstraint
modeCase a b cs = ModeCase (typeAnnotation a) (typeAnnotation b) (map typeAnnotation cs)

data ModeAtomConstraint = ModeAtomImpl [(ModeAtom,ModeAtom)] [(ModeAtom,ModeAtom)]
                        | ModeAtomMax  ModeAtom [ModeAtom]
                        deriving (Show,Eq,Data,Typeable)

modeAtomConstraints :: [ModeConstraint] -> [ModeAtomConstraint]
modeAtomConstraints = collapseMax . concatMap go
  where
    go (ModeImpl ps c)   = goImpl ps c
    go (ModeCase e d bs) = goCase (topmostMode d) e bs
    go (ModeSup e bs)    = goSup e bs

    goImpl ps c = [ ModeAtomImpl (concatMap go ps) (go c) ]
      where
        go (ModeFixpoint, ModeFixpoint) = []
        go (m1          , m2          ) = c:cs
          where
            c  = (topmostMode m1, topmostMode m2)
            cs = goSubmodes (submodes m1) [submodes m2]
               $ \m1s m2s   -> goSubmodes m1s m2s 
               $ \m1' [m2'] -> go (m1', m2')

    goSup ModeFixpoint bs = assert (all (== ModeFixpoint) bs) []
    goSup m            bs = c:cs
      where 
        c  = ModeAtomMax (topmostMode m) $ map topmostMode bs
        cs = goSubmodes (submodes m) (map submodes bs)
           $ \ms bss -> goSubmodes ms bss goSup

    goCase _ ModeFixpoint bs = assert (all (== ModeFixpoint) bs) []
    goCase d e            bs = c:cs
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
