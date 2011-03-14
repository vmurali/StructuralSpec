module PrintModule(printModule) where

import Text.Regex
import Data.Maybe
import Data.List

import DataTypes

getFields ifcName fileIfcs = (interfaceFields . fromJust) $ foldl (matchInterface ifcName) Nothing fileIfcs
 where
  matchInterface _       (Just found) _            = Just found
  matchInterface ifcName Nothing      (file, ifcs) = find (\x -> interfaceName x == ifcName) ifcs

prefixModule body field = subRegex (mkRegex (nonWordNonDot ++ field ++ nonWord)) body ("\\1(tpl_1(_))." ++ field ++ "\\2")
 where
  nonWordNonDot = "([^A-Za-z0-9_\\.]|^)"
  nonWord       = "([^A-Za-z0-9_]|$)"

modifyBody body ifcName fileIfcs = replaceArrow $ foldl prefixModule body (map (\x -> fieldName x) $ getFields ifcName fileIfcs)
 where
  replaceArrow str = subRegex (mkRegex ":=") str "<="

printModule fileIfcs (Module name args ifcName ifcArgs provisos body) =
  "module " ++ name ++ args ++ "(" ++ ifcName ++ ifcArgs ++ ") " ++ provisos ++ ";\n" ++
  "  Tuple2#(" ++ ifcName ++ ifcArgs ++ ", " ++ ifcName ++ ifcArgs ++ "_)" ++ "_ <- " ++ "_" ++ ifcName ++ "(False, ?, False, ?, True, True);\n" ++
     modifyBody body ifcName fileIfcs ++
  "  return tpl_2(_);\n" ++
  "endmodule\n\n"
