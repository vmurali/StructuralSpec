module ParseGeneric(parseGeneric) where

import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Combinator
import Text.ParserCombinators.Parsec.Prim

import DataTypes
import Lexer

parseGeneric = do
  xs <- manyTill1 anyChar $ (try $ reserved "import") <|> (try $ reserved "interface") <|> (try $ reserved "module") <|> eof
  return $ Generic xs

