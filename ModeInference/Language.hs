{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
module ModeInference.Language
where

import Data.Data (Data,Typeable)

data Program a = Program (Binding a) [Declaration a]
                 deriving (Show,Eq,Data,Typeable,Functor)

data Declaration a = DeclBind (Binding a)
                   | DeclAdt Adt
                   deriving (Show,Eq,Data,Typeable,Functor)

data Binding a = Binding { bindName       :: TypedIdentifier a 
                         , bindParameters :: [TypedIdentifier a]
                         , bindExpression :: Expression a
                         }
               deriving (Show,Eq,Data,Typeable,Functor)

type Identifier = String

data TypedIdentifier a = TypedIdentifier 
  { identifier :: Identifier
  , idType     :: a
  }
  deriving (Show,Eq,Data,Typeable,Functor)

data Expression a = ExpVar  (TypedIdentifier a)
                  | ExpCon  (TypedIdentifier a)
                  | ExpApp  (Expression a) [Expression a]
                  | ExpCase (Expression a) [Branch a]
                  | ExpLet  (Binding a) (Expression a)
                  deriving (Show,Eq,Data,Typeable,Functor)

data Branch a = Branch { branchPattern    :: Pattern a
                       , branchExpression :: Expression a
                       }
                       deriving (Show,Eq,Data,Typeable,Functor)

data Pattern a = PatCon Identifier [TypedIdentifier a]
               | PatVar (TypedIdentifier a)
               deriving (Show,Eq,Data,Typeable,Functor)

data Adt = Adt { adtName         :: Identifier
               , adtConstructors :: [Constructor]
               }
               deriving (Show,Eq,Data,Typeable)

data Constructor = Constructor { conName       :: Identifier
                               , conParameters :: [Type]
                               }
                 deriving (Show,Eq,Data,Typeable)

data AnnotatedType a = AnnotatedType { typeIdentifier :: Identifier
                                     , typeAnnotation :: a 
                                     }
                     | FunctionType [AnnotatedType a] (AnnotatedType a)
                     deriving (Show,Eq,Ord,Data,Typeable,Functor)

data ModeAtom = Known
              | Unknown
              | ModeVar Identifier
              deriving (Show,Eq,Ord,Data,Typeable)

data Mode = Mode { topmostMode :: ModeAtom
                 , submodes    :: [[Mode]]
                 }
          | ModeFixpoint
          deriving (Show,Eq,Ord,Data,Typeable)

type Type   = AnnotatedType ()
type MType  = AnnotatedType Mode
