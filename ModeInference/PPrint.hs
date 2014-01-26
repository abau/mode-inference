{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module ModeInference.PPrint 
where

import           Data.List (intersperse)
import           Text.PrettyPrint hiding (Mode)
import           ModeInference.Language
import           ModeInference.Constraint (ModeConstraint(..),ModeAtomConstraint(..))

class PPrint a where
  pprint :: a -> Doc

instance PPrint Identifier where 
  pprint = text

instance PPrint ModeAtom where
  pprint Unknown = char '?'
  pprint Known   = char '!'

instance PPrint Mode where
  pprint ModeFixpoint = text "fixpoint"
  pprint (Mode m mss) = parens $ pprint m <> text ", " <> rest
    where
      rest  = brackets $ hcat $ punctuate (text ",") $ map go     mss
      go ms = brackets $ hcat $ punctuate (text ",") $ map pprint ms

instance PPrint a => PPrint (AnnotatedType a) where
  pprint (AnnotatedType id ann) = pprint id <> char '^' <> pprint ann

  pprint (FunctionType as r) = hsep $ (pprint "->") : (map pprint $ as ++ [r])

instance PPrint a => PPrint (TypedIdentifier a) where
  pprint (TypedIdentifier id t) = pprint id <> (brackets $ pprint t)

instance PPrint () where
  pprint = const empty

instance PPrint Constructor where
  pprint (Constructor id ps) = hsep $ (pprint id) : (map pprint ps)

instance PPrint Adt where
  pprint (Adt id cons) = 
    (text "data" <+> pprint id <+> text "=")
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

instance PPrint ModeConstraint where
  pprint (ModeEq m1 m2) = pprint m1 <+> text "=" <+> pprint m2
  pprint (ModeSup m ms) = pprint m  <+> text "= supremum (" 
                                    <> (hcat $ punctuate (text ", ") $ map pprint ms) 
                                    <> text ")"
  pprint (ModeCase e d bs) = 
        text "if top-most (" <> pprint d <> text ") == ? then " <> pprint e <> text " in max-unknown"
     $$ text "if top-most (" <> pprint d <> text ") == ! then " <> pprint e <> text " = supremum (" 
     <> (hcat $ punctuate (text ", ") $ map pprint bs) 
     <> text ")"

instance PPrint [ModeConstraint] where
  pprint = vcat . map pprint

instance PPrint ModeAtomConstraint where
  pprint (ModeAtomEq m1 m2) = pprint m1 <+> text "=" <+> pprint m2
  pprint (ModeAtomMax m ms) = pprint m  <+> text "= max (" 
                                        <> (hcat $ punctuate (text ", ") $ map pprint ms) 
                                        <> text ")"

instance PPrint [ModeAtomConstraint] where
  pprint = vcat . map pprint

