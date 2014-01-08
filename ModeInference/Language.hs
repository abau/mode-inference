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

data Binding a = Binding { bindName       :: AnnIdentifier a 
                         , bindParameters :: [AnnIdentifier a]
                         , bindExpression :: Expression a
                         }
               deriving (Show,Eq,Data,Typeable,Functor)

type Identifier = String

data AnnIdentifier a = AnnIdentifier 
  { annId           :: Identifier
  , annIdAnnotation :: a
  }
  deriving (Show,Eq,Data,Typeable,Functor)

data Expression a = ExpVar  (AnnIdentifier a)
                  | ExpCon  (AnnIdentifier a)
                  | ExpApp  (Expression a) [Expression a]
                  | ExpCase (Expression a) [Branch a]
                  | ExpLet  [Binding a] (Expression a)
                  deriving (Show,Eq,Data,Typeable,Functor)

data Branch a = Branch { branchPattern    :: Pattern a
                       , branchExpression :: Expression a
                       }
                       deriving (Show,Eq,Data,Typeable,Functor)

data Pattern a = PatCon Identifier [AnnIdentifier a]
               | PatVar (AnnIdentifier a)
               deriving (Show,Eq,Data,Typeable,Functor)

data Adt = Adt { adtName         :: Identifier
               , adtVariables    :: [Identifier]
               , adtConstructors :: [Constructor]
               }
               deriving (Show,Eq,Data,Typeable)

data Constructor = Constructor { conName      :: Identifier
                               , conArguments :: [ConstructorArgument]
                               }
                 deriving (Show,Eq,Data,Typeable)

data ConstructorArgument = ConsArgRec
                         | ConsArgVar Identifier
                         deriving (Show,Eq,Data,Typeable)

data Type = Type Identifier [Type]
          deriving (Show,Eq,Data,Typeable)

data MType = MType Identifier Mode [MType]
           deriving (Show,Eq,Ord,Data,Typeable)

data Mode = Unknown
          | Known
          deriving (Show,Eq,Ord,Data,Typeable)


