module Interface(parseInterface) where

import Text.ParserCombinators.Parsec.Prim
import Text.ParserCombinators.Parsec.Combinator
import Text.ParserCombinators.Parsec.Char

import DataTypes
import Lexer

parseInterface = do
  try $ reserved "interface"
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
  let field = Field
              { fieldType = fieldType
              , fieldArgs = args
              , fieldIndices = indices
              , fieldName = name
              , fieldEn1 = []
              , fieldEn2 = []
              , fieldGuard1 = []
              , fieldGuard2 = []
              }
  modField <- parseAttributes (return field)
  return modField

parseAttributes fieldMonad = do
  field <- fieldMonad
  (    do
         try $ reserved "en"
         xs <- parseAttributeNames
         parseAttributes $ return field{fieldEn1=xs}
       )
   <|> (do
          try $ reserved "enRev"
          xs <- parseAttributeNames
          parseAttributes $ return field{fieldEn2=xs}
       )
   <|> (do
          try $ reserved "guard"
          xs <- parseAttributeNames
          parseAttributes $ return field{fieldGuard1=xs}
       )
   <|> (do
          try $ reserved "guardRev"
          xs <- parseAttributeNames
          parseAttributes $ return field{fieldGuard2=xs}
       )
   <|> (do
          semi
          return field
        )

parseAttributeNames = lexeme $ between (char '(') (char ')') (sepBy1 identifier comma)
