{-# LANGUAGE LambdaCase #-}
module ModeInference.Type where

import ModeInference.Language
import {-# SOURCE #-} ModeInference.Semantic (supremum)
import ModeInference.Util (makeMaxUnknown)

mtypeOf :: Expression MType -> MType
mtypeOf = \case 
  ExpVar v     -> annIdAnnotation v
  ExpCon v     -> annIdAnnotation v
  ExpApp f _   -> case mtypeOf f of
                    MType "->" Known ts -> last ts
  ExpCase d bs -> case mtypeOf d of
    MType _ Unknown _ -> makeMaxUnknown $ mtypeOf $ branchExpression $ head bs
    _                 -> supremum $ map (mtypeOf . branchExpression) bs
  ExpLet _ a   -> mtypeOf a
