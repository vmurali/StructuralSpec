module ParsePort(parsePort) where

import Text.ParserCombinators.Parsec

import DataTypes
import Lexer

parsePort = do
  try $ reserved "port"
  name <- identifier
  args <-
    (do
       (try . lexeme) $ char '#'
       parens $ sepBy1 parseArg comma
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
  reverse <- optional "Reverse"
  fieldT <- identifier
  args <- parensBalancedPrefixed "" $ char '#'
  indices <- parseIndices
  name <- identifier
  writeGuard <- parseAttribute "WriteGuard"
  readGuard <- parseAttribute "ReadGuard"
  semi
  return Field
    { fieldReverse = reverse
    , fieldType = fieldT
    , fieldArgs = args
    , fieldIndices = indices
    , fieldName = name
    , fieldWriteGuard = writeGuard
    , fieldReadGuard = readGuard
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
      parens identifier
  ) <|> return []
