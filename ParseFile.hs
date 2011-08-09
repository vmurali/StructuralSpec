module ParseFile(parseFile) where

import Text.ParserCombinators.Parsec

import DataTypes
import ParseInclude
import ParsePort
import ParsePartition
import ParseInstance
import ParseAlias
import ParseGeneric

parseFile = many $ parseInclude <|> parsePort <|> parseInstance <|> parsePartition <|> parseAlias <|> parseGeneric
