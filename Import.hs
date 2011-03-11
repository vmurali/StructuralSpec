module Import(parseImport) where

import Text.ParserCombinators.Parsec.Prim

import Lexer
import DataTypes

parseImport = do
  try $ reserved "import"
  file <- identifier
  semi
  return $ Import file
