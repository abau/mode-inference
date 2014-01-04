{-# LANGUAGE LambdaCase #-}
module ModeInference.Type where

import ModeInference.Language

mtypeOf :: Expression MType -> MType
mtypeOf = \case 
  ExpVar v     -> annIdAnnotation v
  ExpCon v     -> annIdAnnotation v
  ExpApp f _   -> resultingMType $ mtypeOf f
  ExpCase _ bs -> mtypeOf $ branchExpression $ head bs
  ExpLet _ a   -> mtypeOf a

resultingMType :: MType -> MType
resultingMType (MType "->" Known ts) = last ts
