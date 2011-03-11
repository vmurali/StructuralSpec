module Module(parseModule) where

import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Combinator
import Text.ParserCombinators.Parsec.Prim

import DataTypes
import Lexer

parseHeader = do
  try $ reserved "module"
  name <- identifier
  args <- parseParams
  reserved "implements"
  implementName <- identifier
  implementArgs <- parseParams
  hasProvisos <- optionMaybe $ reserved "provisos"
  provisos <- case hasProvisos of
                Just _  -> do{x <- lexeme parensBalanced; return $ "provisos" ++ x;}
                Nothing -> return []
  char ';'
  return Module
         { moduleName = name
         , moduleArgs = args
         , implementName = implementName
         , implementArgs = implementArgs
         , moduleProvisos = provisos
         , moduleBody = ""
         }

parseModule = do
  header <- parseHeader
  body   <- manyTill anyChar $ try (reserved "endmodule")
  return header
         { moduleBody = body
         }
