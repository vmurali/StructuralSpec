module PrintAlias(printAlias) where

import DataTypes

printAlias (Alias name params port portParams) = return $ 
  "typedef " ++ port ++ portParams ++ " " ++ name ++ params ++ ";\n" ++
  "typedef " ++ port ++ "_" ++ portParams ++ " " ++ name ++ "_" ++ params ++ ";\n\n"
