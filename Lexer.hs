module Lexer where

import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Combinator
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Prim
import qualified Text.ParserCombinators.Parsec.Token as P

lexer = P.makeTokenParser defn

defn = P.LanguageDef {
  P.commentStart = "",
  P.commentEnd = "",
  P.commentLine = "",
  P.nestedComments = False,
  P.opStart = undefined,
  P.opLetter = undefined,
  P.reservedOpNames = undefined,

  P.identStart      = letter,
  P.identLetter     = alphaNum <|> char '_',

  P.reservedNames   = [ "include"
                      , "port"
                      , "endport"
                      , "partition"
                      , "endpartition"
                      , "Guard"
                      , "GuardRev"
                      , "Reverse"
                      ],

  P.caseSensitive   = True
}

whiteSpace = P.whiteSpace lexer
brackets   = P.brackets lexer
semi       = P.semi lexer
comma      = P.comma lexer
colon      = P.colon lexer
identifier = P.identifier lexer
lexeme     = P.lexeme lexer
reserved   = P.reserved lexer    
reservedOp = P.reservedOp lexer
parens     = P.parens lexer

parensBalanced = do
  char '('
  firsts <- allChar
  mids <- many $ do{x <- parensBalanced; y <- allChar; return $ x ++ y;}
  char ')'
  return $ "(" ++ firsts ++ concat mids ++ ")"
  where
    allChar = manyTill anyChar $ (try . lookAhead . oneOf) "()"

parensBalancedPrefixed prefix p =
  (do
     try $ lexeme p
     xs <- lexeme parensBalanced
     return $ prefix ++ xs
  ) <|> return ""
     
parseParams = parensBalancedPrefixed "#" $ char '#'

parseIndices = (many . brackets . many . noneOf) "[]"

manyTill1 p e = do
  xs <- manyTill p e
  case xs of
    [] -> pzero
    _  -> return xs

betweenParens p = between (lexeme $ char '(') (lexeme $ char ')') p

parseProvisos = parensBalancedPrefixed "provisos" $ reserved "provisos"
