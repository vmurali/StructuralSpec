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

  P.reservedNames   = [ "interface"
                      , "endinterface"
                      , "module"
                      , "endmodule"
                      , "implements"
                      , "rule"
                      , "connect"
                      , "en"
                      , "enRev"
                      , "guard"
                      , "guardRev"
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
     try p
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
