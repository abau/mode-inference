{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
module ModeInference.Constraint
where

import           Control.Exception (assert)
import qualified Data.Map as M
import           Data.List (partition,transpose)
import           Data.Generics (Data,Typeable)
import           ModeInference.Language
import           ModeInference.Syntax (topmost)

data MTypeConstraint = MTypeImpl [(MType,MType)] (MType,MType)
                     | MTypeSup  MType [MType]
                     | MTypeCase MType MType [MType]
                     deriving (Show,Eq,Data,Typeable)

data ModeConstraint = ModeMax Mode [Mode]
                    | ModeImpl [(Mode,Mode)] (Mode,Mode)
                    deriving (Show,Eq,Data,Typeable)

modeConstraints :: [MTypeConstraint] -> [ModeConstraint]
modeConstraints = collapseMax . concatMap go
  where
    go (MTypeImpl ps c)   = goImpl ps c
    go (MTypeCase e d bs) = goCase (topmost d) e bs
    go (MTypeSup e bs)    = goSup e bs

    goImpl ps c = map (ModeImpl $ concatMap go ps) $ go c
      where
        go (MTypeSelf, MTypeSelf) = []
        go (t1, t2)               = x:xs
          where
            x  = (topmost t1, topmost t2)
            xs = goConstructors (\t1' [t2'] -> go (t1', t2')) t1 [t2]

    goSup MTypeSelf bs = assert (all (== MTypeSelf) bs) []
    goSup m         bs = x : xs
      where 
        x  = ModeMax (topmost m) $ map topmost bs
        xs = goConstructors goSup m bs

    goCase _ MTypeSelf bs = assert (all (== MTypeSelf) bs) []
    goCase d e         bs = x : xs
      where
        x  = ModeMax (topmost e) $ d : (map topmost bs)
        xs = goConstructors (goCase d) e bs

    goConstructors :: (MType -> [MType] -> [a]) -> MType -> [MType] -> [a]
    goConstructors _ MTypeSelf ts = assert (all (== MTypeSelf) ts) []
    goConstructors f t         ts = assert (all (\cs -> length cs == length cons) conss) $
        concat 
      $ zipWith (goConstructor f) cons
      $ transpose conss
      where 
        MType _ _ cons = t
        conss          = map (\(MType _ _ cons) -> cons) ts

    goConstructor :: (MType -> [MType] -> [a]) -> MTypeConstructor -> [MTypeConstructor] -> [a]
    goConstructor f c cs = assert (all (\ps -> length ps == length cParams) csParams) $
        concat 
      $ zipWith f cParams
      $ transpose csParams
      where
        cParams  =     mtypeConParameters c
        csParams = map mtypeConParameters cs

    collapseMax constraints = max' ++ rest
      where
        (max, rest) = partition isMax constraints
        max'        = map (\(i,ms) -> ModeMax i ms)
                    $ M.toList
                    $ M.fromListWith (++) 
                    $ map (\(ModeMax i ms) -> (i,ms)) max

    isMax (ModeMax {}) = True
    isMax _            = False
