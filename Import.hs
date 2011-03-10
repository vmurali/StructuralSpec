module Import(parseImport) where

import Lexer
import DataTypes

parseImport = do
  reserved "import"
  file <- identifier
  semi
  return $ Import file
