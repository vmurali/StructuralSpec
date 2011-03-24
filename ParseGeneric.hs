module ParseGeneric(parseGeneric) where

import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Combinator
import Text.ParserCombinators.Parsec.Prim

import DataTypes
import Lexer

avoids =
  (try $ seps >> reserved "include") <|>
  (try $ seps >> reserved "port") <|>
  (try $ seps >> reserved "partition") <|>
  eof
 where
  seps = char ';' <|> space

parseGeneric = do
  xs <- manyTill1 anyChar $ lookAhead avoids
  final <- anyChar <|> return '\n'
  return $ Generic (xs ++ [final])

