{-# LANGUAGE LambdaCase #-}
module ModeInference.Type where

import ModeInference.Language
import {-# SOURCE #-} ModeInference.Semantic (supremum)
import ModeInference.Util (makeMaxUnknown)

mtypeOf :: Expression MType -> MType
mtypeOf = \case 
  ExpVar v     -> annIdAnnotation v
  ExpCon v     -> annIdAnnotation v
  ExpApp f _   -> resultMType $ mtypeOf f
  ExpCase d bs -> case mtypeOf d of
    MType _ Unknown _ -> makeMaxUnknown $ mtypeOf $ branchExpression $ head bs
    MType _ Known   _ -> supremum $ map (mtypeOf . branchExpression) bs
  ExpLet _ a   -> mtypeOf a

resultType :: Type -> Type
resultType = \case 
  Type "->" ts -> last ts
  type_        -> type_

resultMType :: MType -> MType
resultMType = \case 
  MType "->" _ ts -> last ts
  mtype           -> mtype
