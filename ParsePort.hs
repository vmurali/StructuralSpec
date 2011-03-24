module ParsePort(parsePort) where

import Text.ParserCombinators.Parsec.Prim
import Text.ParserCombinators.Parsec.Combinator
import Text.ParserCombinators.Parsec.Char

import DataTypes
import Lexer

parsePort = do
  try $ reserved "port"
  name <- identifier
  args <-
    (do
       (try . lexeme) $ char '#'
       betweenParens $ sepBy1 parseArg comma
    ) <|> (return [])
  semi
  fields <- many parseField
  reserved "endport"
  return Port
    { portName = name
    , portArgs = args
    , portFields = fields
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
