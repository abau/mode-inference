{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module ModeInference.PPrint 
where

import Data.List (intersperse)
import Text.PrettyPrint hiding (Mode)
import ModeInference.Language
import ModeInference.Constraint

class PPrint a where
  pprint :: a -> Doc

instance PPrint Identifier where 
  pprint = text

instance PPrint Mode where
  pprint Unknown        = char '?'
  pprint Known          = char '!'

instance PPrint IMode where
  pprint (Mode  m) = pprint m
  pprint (IMVar v) = pprint v

instance PPrint a => PPrint (AnnotatedType a) where
  pprint (AnnotatedType "->" _ ts) = hsep $ (pprint "->") : (map (parens . pprint) ts)
  pprint (AnnotatedType id ann ts) = hsep $ (pprint id <> char '^' <> pprint ann)
                                          : (map (parens . pprint) ts)

instance PPrint a => PPrint (TypedIdentifier a) where
  pprint (TypedIdentifier id t) = pprint id <> (brackets $ pprint t)

instance PPrint () where
  pprint = const empty

instance PPrint ConstructorArgument where
  pprint ConsArgRec     = text "rec"
  pprint (ConsArgVar v) = pprint v

instance PPrint Constructor where
  pprint (Constructor id vs) = hsep $ (pprint id) : (map pprint vs)

instance PPrint Adt where
  pprint (Adt id vs cons) = 
    (hsep ( [text "data", pprint id] ++ (map pprint vs) ) <+> text "=")
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
  pprint (ExpLet bs e) = text "let" <+> 
    (braces $ nest 4 $ vcat $ punctuate (text " ; ") $ map pprint bs)
    $$ text "in" $$ nest 2 (pprint e)

instance PPrint a => PPrint (Binding a) where
  pprint (Binding id ps e) = (hsep $ (pprint id) : (map pprint ps)) 
    <+> text "=" $$ nest 2 (pprint e)

instance PPrint a => PPrint (Declaration a) where
  pprint (DeclBind b) = pprint b
  pprint (DeclAdt a)  = pprint a

instance PPrint a => PPrint (Program a) where
  pprint (Program m ds) = 
    vcat $ intersperse (text ";") $ map pprint $ DeclBind m : ds

instance PPrint IMTypeConstraint where
  pprint (IMTypeEq a b)  = pprint a <+> text "=" <+> pprint b
  pprint (IMTypeSup a b) = 
    pprint a <+> text "= supremum (" <> (hcat $ punctuate (text ", ") $ map pprint b) 
                                     <> text ")"
  pprint (IMTypeCase e d b) = 
       text "if top-most (" <> pprint d <> text ") == ? then " <> pprint e <> text " in max-unknown"
    $$ text "if top-most (" <> pprint d <> text ") == ! then " <> pprint e <> text " = supremum (" <> 
          (hcat $ punctuate (text ", ") $ map pprint b) <> text ")"

instance PPrint IModeConstraint where
  pprint (IModeEq a b)  = pprint a <+> text "=" <+> pprint b
  pprint (IModeMax a b) = 
    pprint a <+> text "= max (" <> (hcat $ punctuate (text ", ") $ map pprint b) 
                                <> text ")"

instance PPrint [IMTypeConstraint] where
  pprint = vcat . map pprint

instance PPrint [IModeConstraint] where
  pprint = vcat . map pprint
