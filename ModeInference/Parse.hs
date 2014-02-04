{-# LANGUAGE LambdaCase #-}
module ModeInference.Parse 
where

import           Control.Monad (when)
import           Data.Char (isLower,isUpper)
import           Data.List (find,delete) 
import           Text.Parsec hiding (runParser, parse)
import qualified Text.Parsec as P
import           Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as T
import qualified Text.Parsec.Language as L
import           ModeInference.Language hiding (identifier)

parse :: (Show a) => Parser a -> String -> a
parse p input = case P.parse (toplevel p) "ModeInference.runParser" input of
  Left err -> error $ show err
  Right a  -> a

parseFile :: (Show a) => Parser a -> FilePath -> IO a
parseFile p file = do
  content <- readFile file
  case P.parse (toplevel p) file content of
    Left err -> error $ show err
    Right a  -> return a

parseArgumentMTypes :: String -> [MType]
parseArgumentMTypes = parse $ sepBy1 mtype $ reservedOp ","

toplevel :: Parser a -> Parser a
toplevel p = do
  whiteSpace
  x <- p
  eof
  return x

program :: Parser (Program Type)
program = do
  ds <- sepBy1 declaration $ reservedOp ";"

  let Just (DeclBind main) = find (\case 
        DeclBind (Binding (TypedIdentifier "main" _) _ _) -> True
        _                                                 -> False) ds
      rest = DeclBind main `delete` ds
  return $ Program main rest

declaration :: Parser (Declaration Type)
declaration = declAdt <|> declBind <?> "declaration"
  where
    declBind = binding >>= return . DeclBind
    declAdt  = adt >>= return . DeclAdt

binding :: Parser (Binding Type)
binding = do
  (f:xs) <- many1 $ typedIdentifier variableIdentifier
  reservedOp "="
  exp <- expression
  return $ Binding f xs exp

expression :: Parser (Expression Type)
expression = try expApp <|> expVar <|> expCon <|> expCase <|> expLet <?> "expression"
  where
    expVar = typedIdentifier variableIdentifier    >>= return . ExpVar
    expCon = typedIdentifier constructorIdentifier >>= return . ExpCon
    expApp = do 
      f  <- expVar <|> expCon
      as <- many1 $ choice [expVar, expCon, parens $ expression]
      return $ ExpApp f as

    expCase = do
      reserved "case"
      exp <- expression
      reserved "of"
      branches <- braces $ sepBy1 branch $ reservedOp ";"
      return $ ExpCase exp branches

    expLet = do
      reserved "let"
      b <- binding
      reserved "in"
      exp <- expression
      return $ ExpLet b exp

branch :: Parser (Branch Type)
branch = do
  p <- pattern
  reservedOp "->"
  exp <- expression
  return $ Branch p exp

pattern :: Parser (Pattern Type)
pattern = patVar <|> patCon <?> "pattern"
  where
    patVar = typedIdentifier variableIdentifier >>= return . PatVar
    patCon = do
      c  <- constructorIdentifier
      vs <- many $ typedIdentifier variableIdentifier
      return $ PatCon c vs

typedIdentifier :: Parser Identifier -> Parser (TypedIdentifier Type)
typedIdentifier pId = do
  id <- pId
  t  <- brackets type_
  return $ TypedIdentifier id t

adt :: Parser Adt
adt = do
  reserved "data"
  id <- nonFunTypeIdentifier
  reservedOp "="
  cons <- sepBy1 constructor $ reservedOp "|"
  return $ Adt id cons

constructor :: Parser Constructor
constructor = do
  id <- constructorIdentifier 
  ps <- many nonFunctionType
  return $ Constructor id ps

type_ :: Parser Type
type_ = nonFunctionType <|> functionType <?> "type"

nonFunctionType :: Parser Type
nonFunctionType =  ( reserved "self"      >>  return   TypeSelf )
               <|> ( nonFunTypeIdentifier >>= return . Type )
               <?> "non-functional type"

functionType :: Parser Type
functionType = do
  funTypeIdentifier
  ts <- many1 $ choice [nonFunctionType, parens nonFunctionType]
  let l = length ts
  return $ FunctionType (take (l-1) ts) (last ts)

constructorIdentifier :: Parser Identifier
constructorIdentifier = try uppercaseIdentifier <?> "constructor identifier"

nonFunTypeIdentifier :: Parser Identifier
nonFunTypeIdentifier = try uppercaseIdentifier <?> "non-functional type identifier"

funTypeIdentifier :: Parser Identifier
funTypeIdentifier = reservedOp "->" >> return "->"

variableIdentifier :: Parser Identifier
variableIdentifier = try lowercaseIdentifier <?> "variable identifier"

uppercaseIdentifier :: Parser Identifier
uppercaseIdentifier = do
  id <- identifier
  when (isLower $ head id) $
    fail ""
  return id

lowercaseIdentifier :: Parser Identifier
lowercaseIdentifier = do
  id <- identifier
  when (isUpper $ head id) $
    fail ""
  return id

mtype :: Parser MType
mtype = nonFunctionMType <|> functionMType <?> "mtype"

nonFunctionMType :: Parser MType
nonFunctionMType =  ( reserved "self" >> return MTypeSelf )
                <|> ( do id   <- nonFunTypeIdentifier 
                         m    <- mode
                         cons <- braces $ sepBy1 mtypeConstructor $ reservedOp ";"
                         return $ MType id m cons
                    )
                <?> "non-functional mtype"

functionMType :: Parser MType
functionMType = do
  funTypeIdentifier
  ts <- many1 $ choice [nonFunctionMType, parens nonFunctionMType]
  let l = length ts
  return $ FunctionMType (take (l-1) ts) (last ts)

mtypeConstructor :: Parser MTypeConstructor
mtypeConstructor = do
  id <- constructorIdentifier
  ts <- many $ choice [nonFunctionMType, parens nonFunctionMType]
  return $ MTypeConstructor id ts

mode :: Parser Mode
mode = choice 
  [ reservedOp "?" >> return Unknown
  , reservedOp "!" >> return Known
  ]

identifier :: Parser Identifier
identifier = T.identifier lexer

lexer :: T.TokenParser ()
lexer = T.makeTokenParser $ L.emptyDef
  { T.reservedOpNames = ["=", ";", "^", "?", "!", "->", "|", ",", "(", ")", "[", "]", "{", "}"]
  , T.reservedNames   = ["let", "in", "case", "of", "data", "self"]
  , T.opStart         = fail ""
  , T.opLetter        = fail ""
  }

whiteSpace = T.whiteSpace lexer
reservedOp = T.reservedOp lexer
reserved = T.reserved lexer
parens = T.parens lexer
brackets = T.brackets lexer
braces = T.braces lexer
