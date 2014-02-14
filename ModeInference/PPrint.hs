{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module ModeInference.PPrint 
where

import           Data.List (intersperse)
import qualified Data.Map as M
import           Text.PrettyPrint hiding (Mode)
import           ModeInference.Language
import           ModeInference.Constraint (MTypeConstraint(..),ModeConstraint(..))
import           ModeInference.Constraint.Solve (Assignment)
import           ModeInference.Syntax (ModeInstances)

class PPrint a where
  pprint :: a -> Doc

instance PPrint Identifier where 
  pprint = text

instance PPrint a => PPrint (TypedIdentifier a) where
  pprint (TypedIdentifier id t) = pprint id <> (brackets $ pprint t)

instance PPrint () where
  pprint = const empty

instance PPrint Constructor where
  pprint (Constructor id ps) = hsep $ (pprint id) : (map pprint ps)

instance PPrint ConstructorParameter where
  pprint (ConParamType t) = pprint t
  pprint ConParamSelf     = text "self"
  pprint (ConParamVar v)  = pprint v

instance PPrint Adt where
  pprint (Adt id vars cons) = 
    (text "data" <+> pprint id <+> (hsep $ map pprint vars) <+> text "=")
    $$
    (nest 2 $ vcat $ punctuate (text " | ") $ map pprint cons)

instance PPrint a => PPrint (Pattern a) where
  pprint (PatVar v) = pprint v
  pprint (PatCon c ts) = hsep $ (pprint c) : (map pprint ts)

instance PPrint a => PPrint (Branch a) where
  pprint (Branch pat exp) = pprint pat <+> text "->" <+> pprint exp

instance PPrint a => PPrint (Expression a) where
  pprint (ExpVar id)   = pprint id
  pprint (ExpCon id)   = pprint id
  pprint (ExpApp f es) = hsep $ (pprint f) : (map (parens . pprint) es)
  pprint (ExpCase e bs) = text "case" <+> pprint e <+> text "of" <+>
    (braces $ nest 2 $ vcat $ punctuate (text " ; ") $ map pprint bs)
  pprint (ExpLet b e) = text "let" <+> pprint b $$ text "in" $$ nest 2 (pprint e)

instance PPrint a => PPrint (Binding a) where
  pprint (Binding id ps e) = (hsep $ (pprint id) : (map pprint ps)) 
    <+> text "=" $$ nest 2 (pprint e)

instance PPrint a => PPrint (Declaration a) where
  pprint (DeclBind b) = pprint b
  pprint (DeclAdt a)  = pprint a

instance PPrint a => PPrint (Program a) where
  pprint (Program m ds) = 
    vcat $ intersperse (text ";") $ map pprint $ DeclBind m : ds

instance PPrint Type where
  pprint (Type id args)      = pprint id <+> (hsep $ map (parens . pprint) args)
  pprint (FunctionType as r) = hcat $ punctuate (pprint " -> ") $ map pprint $ as ++ [r]

instance PPrint Mode where
  pprint Unknown     = char '?'
  pprint Known       = char '!'
  pprint (ModeVar v) = pprint v

instance PPrint MType where
  pprint (MType id m cons) = pprint id <+> pprint m <+> 
                            (braces $ hcat $ punctuate (text "; ") $ map pprint cons)
  pprint (FunctionMType as r) = hcat $ punctuate (pprint " -> ") $ map pprint $ as ++ [r]

instance PPrint MTypeConstructor where
  pprint (MTypeConstructor id ps) = pprint id <+> (hsep $ map (parens . pprint) ps)

instance PPrint MTypeConstructorParameter where
  pprint (MTypeConParamType t) = pprint t
  pprint MTypeConParamSelf     = text "self"
  
instance PPrint MTypeConstraint where
  pprint (MTypeImpl ps c) =  (hcat $ punctuate (text " /\\ ") $ map pprintEq ps)
                         <+> text "==>" <+> pprintEq c
    where 
      pprintEq (a,b) = pprint a <+> text "=" <+> pprint b

  pprint (MTypeSup m ms) = pprint m  <+> text "= supremum {" 
                                      <> (hcat $ punctuate (text ", ") $ map pprint ms) 
                                      <> text "}"
  pprint (MTypeCase e d bs) = 
        text "if top-most (" <> pprint d <> text ") = ? then " <> pprint e <> text " in max-unknown"
     $$ text "if top-most (" <> pprint d <> text ") = ! then " <> pprint e <> text " = supremum {" 
     <> (hcat $ punctuate (text ", ") $ map pprint bs) 
     <> text "}"

instance PPrint [MTypeConstraint] where
  pprint = vcat . map pprint

instance PPrint ModeConstraint where
  pprint (ModeMax m ms) = pprint m  <+> text "= max {" 
                                     <> (hcat $ punctuate (text ", ") $ map pprint ms) 
                                     <> text "}"
  pprint (ModeImpl ps c) =  (hcat $ punctuate (text " /\\ ") $ map pprintEq ps)
                        <+> text "==>" <+> (pprintEq c)
    where 
      pprintEq (a,b) = pprint a <+> text "=" <+> pprint b

instance PPrint [ModeConstraint] where
  pprint = vcat . map pprint

instance PPrint Assignment where
  pprint = vcat . map go . M.toList
    where
      go (k,v) = pprint k <+> text "->" <+> pprint v

instance PPrint ModeInstances where
  pprint = vcat . map go . M.toList
    where
      go ((i,as),r) = pprint $ TypedIdentifier i $ FunctionMType as r 
