module File(parseFile) where

import Text.ParserCombinators.Parsec.Prim

import DataTypes
import Import
import Interface
import Module
import Generic

parseFile = many $ parseImport <|> parseInterface <|> parseModule <|> parseGeneric
