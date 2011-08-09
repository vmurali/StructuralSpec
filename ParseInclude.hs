module ParseInclude(parseInclude) where

import Text.ParserCombinators.Parsec

import Lexer
import DataTypes

parseInclude = do
  try $ reserved "include"
  file <- identifier
  semi
  return $ Include file
