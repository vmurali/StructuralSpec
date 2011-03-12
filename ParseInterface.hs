module ParseInterface(parseInterface) where

import Text.ParserCombinators.Parsec.Prim
import Text.ParserCombinators.Parsec.Combinator
import Text.ParserCombinators.Parsec.Char

import DataTypes
import Lexer

parseInterface = do
  try $ reserved "interface"
  name <- identifier
  args <-
    (do
       (try . lexeme) $ char '#'
       betweenParens $ many1 parseArg
    ) <|> (return [])
  semi
  fields <- many parseField
  reserved "endinterface"
  return Interface
    { interfaceName = name
    , interfaceArgs = args
    , interfaceFields = fields
    }

parseArg =
   (do
     reserved "numeric"
     reserved "type"
     x <- identifier
     return $ Num x
   ) <|>
   (do
     reserved "type"
     x <- identifier
     return $ Type x
   )

parseField = do
  fieldType <- identifier
  args <- parseParams
  indices <- parseIndices
  name <- identifier
  en <- parseAttribute "en"
  enRev <- parseAttribute "enRev"
  guard <- parseAttribute "guard"
  guardRev <- parseAttribute "guardRev"
  semi
  return Field
    { fieldType = fieldType
    , fieldArgs = args
    , fieldIndices = indices
    , fieldName = name
    , fieldEn = en
    , fieldEnRev = enRev
    , fieldGuard = guard
    , fieldGuardRev = guardRev
    }

parseAttribute str =
  ( do 
      try $ reserved str
      betweenParens $ sepBy1 identifier comma
  ) <|> return []
