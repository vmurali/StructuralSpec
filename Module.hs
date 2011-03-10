module Module(parseModule) where

import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Combinator
import Text.ParserCombinators.Parsec.Prim

import DataTypes
import Lexer

parseHeader = do
  reserved "module"
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

parseTill x = do
  let stop = do {sep <- oneOf " \n\t;"; reserved x}
  xs <- manyTill anyChar ((try . lookAhead) stop)
  sep <- stop
  return xs

parseModule = do
  header <- parseHeader
  body   <- parseTill "endmodule"
  return header
         { moduleBody = body
         }
