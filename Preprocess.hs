module Preprocess(preprocess) where

import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Combinator
import Text.ParserCombinators.Parsec.Prim
import Text.Regex

lineComment = do
  string "//"
  manyTill anyChar (try $ (do{newline; return ()}) <|> eof)

blockComment = do
  string "/*"
  manyTill anyChar (try $ string "*/")

removeComment =
  (try $ do{eof; return ""})
   <|> (try $ do{lineComment; removeComment})
   <|> (try $ do{blockComment; removeComment})
   <|> do{x <- anyChar; xs <- removeComment; return $ x:xs}

preprocess str =
  let Right uncommented = runParser removeComment () "" str
  in
    uncommented
