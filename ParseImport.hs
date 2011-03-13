module ParseImport(parseImport) where

import Text.ParserCombinators.Parsec.Prim

import Lexer
import DataTypes

parseImport = do
  imp <- (do{try $ reserved "import"; return def{importBsv = True};})
         <|> (do{try $ reserved "importBsv"; return def;})
  file <- identifier
  semi
  return $ imp{importName = file}
  where def = Import{importName = undefined, importBsv = False}
