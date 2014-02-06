{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
module ModeInference.Constraint
where

import Control.Exception (assert)
import Data.List (transpose)
import Data.Generics (Data,Typeable)
import ModeInference.Language
import ModeInference.Syntax (topmost)

data MTypeConstraint = MTypeImpl [(MType,MType)] (MType,MType)
                     | MTypeSup  MType [MType]
                     | MTypeCase MType MType [MType]
                     deriving (Show,Eq,Data,Typeable)

data ModeConstraint = ModeLT   Mode Mode
                    | ModeImpl [(Mode,Mode)] [(Mode,Mode)]
                    deriving (Show,Eq,Data,Typeable)

modeConstraints :: [MTypeConstraint] -> [ModeConstraint]
modeConstraints = concatMap go
  where
    go (MTypeImpl ps c)   = goImpl ps c
    go (MTypeCase e d bs) = goCase (topmost d) e bs
    go (MTypeSup e bs)    = goSup e bs

    goImpl ps c = [ ModeImpl (concatMap go ps) (go c) ]
      where
        go (t1, t2) = c:cs
          where
            c  = (topmost t1, topmost t2)
            cs = goConstructors (\t1' [t2'] -> go (t1', t2')) t1 [t2]

    goSup m bs = c ++ cs
      where 
        c  = map (flip ModeLT (topmost m)) $ map topmost bs
        cs = goConstructors goSup m bs

    goCase d e bs = c ++ cs
      where
        c  = ModeLT d (topmost e) : (map (flip ModeLT $ topmost e) (map topmost bs))
        cs = goConstructors (goCase d) e bs

    goConstructors :: (MType -> [MType] -> [a]) -> MType -> [MType] -> [a]
    goConstructors f t ts = assert (all (\cs -> length cs == length cons) conss) $
        concat 
      $ zipWith (goConstructor f) cons
      $ transpose conss
      where 
        MType _ _ cons = t
        conss          = map (\(MType _ _ cons) -> cons) ts

    goConstructor :: (MType -> [MType] -> [a]) -> MTypeConstructor -> [MTypeConstructor] -> [a]
    goConstructor f c cs = assert (all (\ps -> length ps == length cParams) csParams) $
        concat 
      $ zipWith (goConstructorParam f) cParams
      $ transpose csParams
      where
        cParams  =     mtypeConParameters c
        csParams = map mtypeConParameters cs

    goConstructorParam :: (MType -> [MType] -> [a]) -> MTypeConstructorParameter 
                       -> [MTypeConstructorParameter] -> [a]

    goConstructorParam _ MTypeConParamSelf ps = assert (all (== MTypeConParamSelf) ps) []
    goConstructorParam f (MTypeConParamType t) ps = f t ts
      where
        ts = map (\(MTypeConParamType t') -> t') ps
