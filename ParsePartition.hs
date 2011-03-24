module ParsePartition(parsePartition) where

import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Combinator
import Text.ParserCombinators.Parsec.Prim

import DataTypes
import Lexer

parsePartition = do
  header <- parseHeader
  body   <- manyTill anyChar $ try (reserved "endpartition")
  return header{partitionBody = body}

parseHeader = do
  try $ reserved "partition"
  name <- identifier
  args <- parseParams
  reserved "implements"
  implementName <- identifier
  implementArgs <- parseParams
  provisos <- parensBalancedPrefixed "provisos" $ reserved "provisos"
  char ';'
  return Partition
         { partitionName = name
         , partitionArgs = args
         , implementName = implementName
         , implementArgs = implementArgs
         , partitionProvisos = provisos
         , partitionBody = ""
         }
