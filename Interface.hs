module Interface(parseInterface) where

import Text.ParserCombinators.Parsec.Prim

import DataTypes
import Lexer

parseInterface = do
  reserved "interface"
  name <- identifier
  args <- parseParams
  semi
  fields <- many parseField
  reserved "endinterface"
  return Interface
         { interfaceName = name
         , interfaceArgs = args
         , interfaceFields = fields
         }

parseField = do
  fieldType <- identifier
  args <- parseParams
  indices <- parseIndices
  name <- identifier
  semi
  return $ Field
           { fieldType = fieldType
           , fieldArgs = args
           , fieldIndices = indices
           , fieldName = name
           }
