module ParseInstance(parseInstance) where

import Text.ParserCombinators.Parsec

import DataTypes
import Lexer

parseInstance = do
  try $ reserved "partinst"
  instanceImplName <- identifier
  instanceImplArgs <- parseParams
  name <- identifier
  args <- parseParams
  provisos <- parseProvisos
  reserved "="
  expr <- manyTill anyChar semi
  return Instance
         { instanceName = name
         , instanceArgs = args
         , instanceImplName = instanceImplName
         , instanceImplArgs = instanceImplArgs
         , instanceProvisos = provisos
         , instanceExpr = expr
         }
