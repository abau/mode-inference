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
import           ModeInference.Language

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

toplevel :: Parser a -> Parser a
toplevel p = do
  whiteSpace
  x <- p
  eof
  return x

program :: Eq a => Parser a -> Parser (Program a)
program ann = do
  ds <- sepBy1 (declaration ann) $ reservedOp ";"

  let Just (DeclBind main) = find (\case 
        DeclBind (Binding (AnnIdentifier "main" _) _ _) -> True
        _                                               -> False) ds
      rest = DeclBind main `delete` ds
  return $ Program main rest

declaration :: Parser a -> Parser (Declaration a)
declaration ann = declAdt <|> declBind <?> "declaration"
  where
    declBind = binding ann >>= return . DeclBind
    declAdt  = adt >>= return . DeclAdt

binding :: Parser a -> Parser (Binding a)
binding ann = do
  (f:xs) <- many1 $ annIdentifier variableIdentifier ann
  reservedOp "="
  exp <- expression ann
  return $ Binding f xs exp

expression :: Parser a -> Parser (Expression a)
expression ann = try expApp <|> expVar <|> expCon <|> expCase <|> expLet <?> "expression"
  where
    expVar = annIdentifier variableIdentifier    ann >>= return . ExpVar
    expCon = annIdentifier constructorIdentifier ann >>= return . ExpCon
    expApp = do 
      f  <- expVar <|> expCon
      as <- many $ choice [expVar, expCon, parens $ expression ann]
      return $ ExpApp f as

    expCase = do
      reserved "case"
      exp <- expression ann
      reserved "of"
      branches <- braces $ sepBy1 (branch ann) $ reservedOp ";"
      return $ ExpCase exp branches

    expLet = do
      reserved "let"
      bs <- many1 $ binding ann
      reserved "in"
      exp <- expression ann
      return $ ExpLet bs exp

branch :: Parser a -> Parser (Branch a)
branch ann = do
  p <- pattern ann
  reservedOp "->"
  exp <- expression ann
  return $ Branch p exp

pattern :: Parser a -> Parser (Pattern a)
pattern ann = patVar <|> patCon <?> "pattern"
  where
    patVar = annIdentifier variableIdentifier ann >>= return . PatVar
    patCon = do
      c  <- constructorIdentifier
      vs <- many $ annIdentifier variableIdentifier ann
      return $ PatCon c vs

annIdentifier :: Parser Identifier -> Parser a -> Parser (AnnIdentifier a)
annIdentifier pId pAnn = do
  id <- pId
  ann <- brackets pAnn
  return $ AnnIdentifier id ann

adt :: Parser Adt
adt = do
  reserved "data"
  id <- nonFunTypeIdentifier
  vs <- many variableIdentifier
  reservedOp "="
  cons <- sepBy1 constructor $ reservedOp "|"
  return $ Adt id vs cons

constructor :: Parser Constructor
constructor = do
  id <- constructorIdentifier 
  as <- many constructorArgument
  return $ Constructor id as

constructorArgument :: Parser ConstructorArgument
constructorArgument = consArgRec <|> consArgVar <?> "constructor argument"
  where
    consArgRec = reserved "rec" >> return ConsArgRec
    consArgVar = variableIdentifier >>= return . ConsArgVar

mtype :: Parser MType
mtype = try typeOperator <|> typeConstant <|> funType <?> "mtype"
  where
    typeConstant = do
      id <- nonFunTypeIdentifier 
      m  <- modeAnn
      return $ MType id m []

    typeOperator = do
      id <- nonFunTypeIdentifier 
      m  <- modeAnn
      ts <- many1 $ choice [typeConstant, parens typeOperator]
      return $ MType id m ts

    funType = do
      id <- funTypeIdentifier
      ts <- many1 $ choice [typeConstant, parens typeOperator]
      return $ MType id Known ts

    modeAnn = reservedOp "^" >> mode

type_ :: Parser Type
type_ = try typeOperator <|> typeConstant <|> funType <?> "type"
  where
    typeConstant = do
      id <- nonFunTypeIdentifier
      return $ Type id []

    typeOperator = do
      id <- nonFunTypeIdentifier
      ts <- many1 $ choice [typeConstant, parens typeOperator]
      return $ Type id ts

    funType = do
      id <- funTypeIdentifier
      ts <- many1 $ choice [typeConstant, parens typeOperator]
      return $ Type id ts

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

mode :: Parser Mode
mode = choice 
  [ reservedOp "?" >> return Unknown
  , reservedOp "!" >> return Known
  ]

identifier :: Parser Identifier
identifier = T.identifier lexer

lexer :: T.TokenParser ()
lexer = T.makeTokenParser $ L.emptyDef
  { T.reservedOpNames = ["=", ";", "^", "?", "!", "->", "|"] 
  , T.reservedNames   = ["let", "in", "case", "of", "data", "rec"]
  , T.opStart         = fail ""
  , T.opLetter        = fail ""
  }

whiteSpace = T.whiteSpace lexer
reservedOp = T.reservedOp lexer
reserved = T.reserved lexer
parens = T.parens lexer
brackets = T.brackets lexer
braces = T.braces lexer
