module ParseImport(parseImport) where

import Text.ParserCombinators.Parsec.Prim

import Lexer
import DataTypes

parseImport = do
  try $ reserved "include"
  file <- identifier
  semi
  return $ Import file
