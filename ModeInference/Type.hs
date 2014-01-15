{-# LANGUAGE LambdaCase #-}
module ModeInference.Type where

import Data.Generics
import ModeInference.Language
import {-# SOURCE #-} ModeInference.Semantic (supremum)

mtypeOf :: Expression MType -> MType
mtypeOf = \case 
  ExpVar v     -> idType v
  ExpCon v     -> idType v
  ExpApp f _   -> resultType $ mtypeOf f
  ExpCase d bs -> case mtypeOf d of
    AnnotatedType _ Unknown _ -> everywhere (mkT $ const Unknown) 
                               $ mtypeOf 
                               $ branchExpression 
                               $ head bs
    AnnotatedType _ Known   _ -> supremum $ map (mtypeOf . branchExpression) bs
  ExpLet _ a   -> mtypeOf a

resultType :: AnnotatedType a -> AnnotatedType a
resultType = \case 
  AnnotatedType "->" _ ts -> last ts
  type_             -> type_

iMTypeFromMType :: MType -> IMType
iMTypeFromMType (AnnotatedType id m ts) = 
  AnnotatedType id (Mode m) $ map iMTypeFromMType ts
