module File(parseFile) where

import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Combinator
import Text.ParserCombinators.Parsec.Prim

import DataTypes
import Import
import Interface
import Module

parseFile =
      (eof >> return [])
  <|> do
        x  <- (parseImport <|>
               parseInterface <|>
               parseModule <|>
               (anyChar >>= return . Generic))
        xs <- parseFile
        return $ x:xs
