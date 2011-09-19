module Preprocess(preprocess) where

import Text.ParserCombinators.Parsec

lineComment = do
  string "//"
  manyTill anyChar (try $ (do{newline; return ()}) <|> eof)

substituteWithSpaces x = case x of
  '\n' -> '\n'
  '\t' -> '\t'
  _    -> ' '

blockComment = do
  string "/*"
  xs <- manyTill anyChar (try $ string "*/")
  return $ map substituteWithSpaces ("/*" ++ xs ++ "*/")

removeComment =
  (try $ do{eof; return ""})
   <|> (try $ do{lineComment; xs <- removeComment; return $ '\n':xs})
   <|> (try $ do{xs <- blockComment; ys <- removeComment; return $ xs ++ ys})
   <|> do{x <- anyChar; xs <- removeComment; return $ x:xs}

preprocess str =
  let Right uncommented = runParser removeComment () "" str
  in
    uncommented
