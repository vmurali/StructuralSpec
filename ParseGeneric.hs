module ParseGeneric(parseGeneric) where

import Text.ParserCombinators.Parsec

import DataTypes
import Lexer

avoids =
  (try $ seps >> reserved "include") <|>
  (try $ seps >> reserved "port") <|>
  (try $ seps >> reserved "partition") <|>
  (try $ seps >> reserved "partinst") <|>
  (try $ seps >> reserved "portalias") <|>
  eof
 where
  seps = char ';' <|> space

parseGeneric = do
  xs <- manyTill1 anyChar $ lookAhead avoids
  final <- anyChar <|> return '\n'
  return $ Generic (xs ++ [final])

