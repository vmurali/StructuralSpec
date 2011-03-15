module ParseModule(parseModule) where

import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Combinator
import Text.ParserCombinators.Parsec.Prim

import DataTypes
import Lexer

parseModule = do
  header <- parseHeader
  body   <- manyTill anyChar $ try (reserved "endmodule")
  return header{moduleBody = body}

parseHeader = do
  try $ reserved "module"
  name <- identifier
  args <- parseParams
  reserved "implements"
  implementReverse <- do{try $ reserved "Reverse"; return True} <|> return False
  implementName <- identifier
  implementArgs <- parseParams
  provisos <- parensBalancedPrefixed "provisos" $ reserved "provisos"
  char ';'
  return Module
         { moduleName = name
         , moduleArgs = args
         , implementReverse = implementReverse
         , implementName = implementName
         , implementArgs = implementArgs
         , moduleProvisos = provisos
         , moduleBody = ""
         }
