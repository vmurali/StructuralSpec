module ParsePartition(parsePartition) where

import Text.ParserCombinators.Parsec

import DataTypes
import Lexer

parsePartition = do
  header <- parseHeader
  body   <- manyTill anyChar $ try (reserved "endpartition")
  return header{partitionBody = body}

parseHeader = do
  try $ reserved "partition"
  implementName <- identifier
  implementArgs <- parseParams
  name <- identifier
  args <- parseParams
  provisos <- parseProvisos
  char ';'
  return Partition
         { partitionName = name
         , partitionArgs = args
         , implementName = implementName
         , implementArgs = implementArgs
         , partitionProvisos = provisos
         , partitionBody = ""
         }
