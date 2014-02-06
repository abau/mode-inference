{-# LANGUAGE LambdaCase #-}
module ModeInference.Type where

import Data.Maybe (mapMaybe)
import ModeInference.Language
import ModeInference.Util (toMaxUnknown)
import {-# SOURCE #-} ModeInference.Semantic (supremum)
import {-# SOURCE #-} ModeInference.Syntax   (topmost)

mtypeOf :: Expression MType -> MType
mtypeOf = \case 
  ExpVar v     -> idType v
  ExpCon v     -> idType v
  ExpApp f _   -> resultMType $ mtypeOf f
  ExpCase d bs -> case topmost $ mtypeOf d of
    Unknown -> toMaxUnknown $ mtypeOf $ branchExpression $ head bs
    Known   -> supremum $ map (mtypeOf . branchExpression) bs

  ExpLet _ a   -> mtypeOf a

argumentTypes :: Type -> [Type]
argumentTypes = \case 
  FunctionType as _ -> as
  _                 -> []

resultType :: Type -> Type
resultType = \case 
  FunctionType _ r -> r
  t                -> t

argumentMTypes :: MType -> [MType]
argumentMTypes = \case 
  FunctionMType as _ -> as
  _                  -> []

resultMType :: MType -> MType
resultMType = \case 
  FunctionMType _ r -> r
  t                 -> t

constructorParameterTypes :: Constructor -> [Type]
constructorParameterTypes = mapMaybe go . conParameters
  where
    go (ConParamType t) = Just t
    go ConParamSelf     = Nothing

constructorParameterMTypes :: MTypeConstructor -> [MType]
constructorParameterMTypes = mapMaybe go . mtypeConParameters
  where
    go (MTypeConParamType t) = Just t
    go MTypeConParamSelf     = Nothing
