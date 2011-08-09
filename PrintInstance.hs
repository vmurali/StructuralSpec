module PrintInstance(printInstance) where

import DataTypes

printInstance (Instance name args implName implArgs provisos expr) = return $
  "\nmodule " ++ name ++ args ++ "(" ++ implName ++ implArgs ++ ")" ++ provisos ++ ";\n" ++
  "  " ++ implName ++ implArgs ++ " " ++ "mod_ <- " ++ expr ++ ";\n" ++
  "  return mod_;\nendmodule\n\n"
