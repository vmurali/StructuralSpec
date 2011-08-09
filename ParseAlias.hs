module ParseAlias(parseAlias) where

import Text.ParserCombinators.Parsec

import DataTypes
import Lexer

parseAlias = do
  try $ reserved "portalias"
  name <- identifier
  params <- parseParams
  lexeme $ char '='
  port <- identifier
  portParams <- parseParams
  semi
  return Alias
    { aliasName = name
    , aliasParams = params
    , aliasPort = port
    , aliasPortParams = portParams
    }
