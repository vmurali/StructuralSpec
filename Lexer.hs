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

  P.opStart        = oneOf "#:",
  P.opLetter       = oneOf "#:=",

  P.identStart      = letter,
  P.identLetter     = alphaNum <|> char '_',

  P.reservedNames   = [ "interface"
                      , "endinterface"
                      , "module"
                      , "endmodule"
                      , "implements"
                      , "rule"
                      , "connect"
                      ],

  P.reservedOpNames = [ "#"
                      , ":="
                      ],

  P.caseSensitive   = True
}

whiteSpace = P.whiteSpace lexer
brackets   = P.brackets lexer
semi       = P.semi lexer
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

optionList p = lexeme $ do
  mayX <- optionMaybe p
  return $ case mayX of
    Just x  -> x
    Nothing -> []

parseParams = optionList $ do
  reservedOp "#"
  xs <- parensBalanced
  return $ "#" ++ xs

parseIndices = (many . brackets . many . noneOf) "[]"
