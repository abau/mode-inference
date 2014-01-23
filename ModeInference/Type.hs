{-# LANGUAGE LambdaCase #-}
module ModeInference.Type where

import Data.Generics
import ModeInference.Language
import {-# SOURCE #-} ModeInference.Semantic (supremumMType)
import {-# SOURCE #-} ModeInference.Syntax   (topmost)

mtypeOf :: Expression MType -> MType
mtypeOf = \case 
  ExpVar v     -> idType v
  ExpCon v     -> idType v
  ExpApp f _   -> resultType $ mtypeOf f
  ExpCase d bs -> case topmost $ mtypeOf d of
    Unknown -> toMaxUnknown $ mtypeOf $ branchExpression $ head bs
    Known   -> supremumMType $ map (mtypeOf . branchExpression) bs

  ExpLet _ a   -> mtypeOf a

resultType :: AnnotatedType a -> AnnotatedType a
resultType = \case 
  FunctionType _ r -> r
  type_            -> type_

toMaxUnknown :: MType -> MType
toMaxUnknown = everywhere $ mkT $ const Unknown 
