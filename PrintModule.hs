module PrintModule(printModule) where

import Text.Regex
import Data.Maybe
import Data.List

import DataTypes

getFields ifcName fileIfcs = (interfaceFields . fromJust) $ foldl (matchInterface ifcName) Nothing fileIfcs
 where
  matchInterface _       (Just found) _            = Just found
  matchInterface ifcName Nothing      (file, ifcs) = find (\x -> interfaceName x == ifcName) ifcs

prefixModule body field = subRegex (mkRegex (nonWordNonDot ++ field ++ nonWord)) body ("\\1(tpl_1(asIfc(_)))." ++ field ++ "\\2")
 where
  nonWordNonDot = "([^A-Za-z0-9_\\.]|^)"
  nonWord       = "([^A-Za-z0-9_]|$)"

modifyBody body ifcName fileIfcs = replaceArrow $ foldl prefixModule body (map (\x -> fieldName x) $ getFields ifcName fileIfcs)
 where
  replaceArrow str = subRegex (mkRegex ":=") str "<="

printModule elastic fileIfcs (Module name args ifcReverse ifcName ifcArgs provisos body) =
  "module " ++ name ++ args ++ "(" ++ ifcName ++ underbar ++ ifcArgs ++ ") " ++ provisos ++ ";\n" ++
  "  Tuple2#(" ++ ifcName ++ ifcArgs ++ ", " ++ ifcName ++ "_" ++ ifcArgs ++ ")" ++ underbar ++ "_ <- " ++ "_" ++ ifcName ++ (if (ifcName == "Output" || ifcName == "Enable") then "(False, ?, True, True);\n" else "") ++
     (if ifcReverse then "  Tuple2#(" ++ ifcName ++ "_" ++ ifcArgs ++ ", " ++ ifcName ++ ifcArgs ++ ")" ++ " _ = tuple2(tpl_2(asIfc(__)), tpl1(asIfc(__)));\n" else "") ++
     modifyBody body ifcName fileIfcs ++
     if elastic then "  rule _r;\n    if(tpl_2(asIfc(isSupplied)))\n      (tpl_2(asIfc(_))).hasBeenUsed;\nendrule\n" else "" ++
  "  return tpl_2(asIfc(_));\n" ++
  "endmodule\n\n"
 where
  underbar = if ifcReverse then "_" else ""
