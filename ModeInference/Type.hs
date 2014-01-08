{-# LANGUAGE LambdaCase #-}
module ModeInference.Type where

import ModeInference.Language
import {-# SOURCE #-} ModeInference.Semantic (supremum)

mtypeOf :: Expression MType -> MType
mtypeOf = \case 
  ExpVar v     -> annIdAnnotation v
  ExpCon v     -> annIdAnnotation v
  ExpApp f _   -> case mtypeOf f of
                    MType "->" Known ts -> last ts
  ExpCase _ bs -> supremum $ map (mtypeOf . branchExpression) bs
  ExpLet _ a   -> mtypeOf a
