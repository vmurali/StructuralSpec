module ParseFile(parseFile) where

import Text.ParserCombinators.Parsec.Prim

import DataTypes
import ParseImport
import ParseInterface
import ParseModule
import ParseGeneric

parseFile = many $ parseImport <|> parseInterface <|> parseModule <|> parseGeneric
