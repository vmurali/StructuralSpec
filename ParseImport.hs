module ParseImport(parseImport) where

import Text.ParserCombinators.Parsec.Prim

import Lexer
import DataTypes

parseImport = do
  bsv <- do{try $ reserved "import"; return False} <|> do{try $ reserved "importBsv"; return True}
  file <- identifier
  semi
  return $ Import
    { importName = file
    , importBsv  = bsv
    }
