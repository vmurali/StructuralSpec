module ParseGeneric(parseGeneric) where

import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Combinator
import Text.ParserCombinators.Parsec.Prim

import DataTypes
import Lexer

parseGeneric = do
  xs <- manyTill1 anyChar $ (try $ reserved "include") <|> (try $ reserved "port") <|> (try $ reserved "partition") <|> eof
  return $ Generic xs

