module ParseFile(parseFile) where

import Text.ParserCombinators.Parsec.Prim

import DataTypes
import ParseInclude
import ParsePort
import ParsePartition
import ParseGeneric

parseFile = many $ parseInclude <|> parsePort <|> parsePartition <|> parseGeneric
