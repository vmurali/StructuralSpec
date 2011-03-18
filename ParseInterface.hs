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
     try $ reserved "numeric"
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
  deflt <- optional "Default"
  reverse <- optional "Reverse"
  fieldT <- identifier
  args <- parensBalancedPrefixed "" $ char '#'
  indices <- parseIndices
  name <- identifier
  en <- parseAttribute "En"
  enRev <- parseAttribute "EnRev"
  guard <- parseAttribute "Guard"
  guardRev <- parseAttribute "GuardRev"
  semi
  return Field
    { fieldDefault = deflt
    , fieldReverse = reverse
    , fieldType = fieldT
    , fieldArgs = args
    , fieldIndices = indices
    , fieldName = name
    , fieldEn = en
    , fieldEnRev = enRev
    , fieldGuard = guard
    , fieldGuardRev = guardRev
    }
  where
    optional str =
      (do
        try $ reserved str
        return True
      ) <|> return False

parseAttribute str =
  ( do 
      try $ reserved str
      betweenParens identifier
  ) <|> return []
