module PrintModule(printModule) where

import Text.Regex
import Data.Maybe
import Data.List

import DataTypes

getFields ifcName fileIfcs = (interfaceFields . fromJust) $ foldl (matchInterface ifcName) Nothing fileIfcs
 where
  matchInterface _       (Just found) _            = Just found
  matchInterface ifcName Nothing      (file, ifcs) = find (\x -> interfaceName x == ifcName) ifcs

prefixModule body field = subRegex (mkRegex (nonWordNonDot ++ field ++ nonWord)) body ("\\1(tpl_1(asIfc(mod)))." ++ field ++ "\\2")
 where
  nonWordNonDot = "([^A-Za-z0-9_\\.]|^)"
  nonWord       = "([^A-Za-z0-9_]|$)"

modifyBody body ifcName fileIfcs = replaceArrow $ foldl prefixModule body (map (\x -> fieldName x) $ getFields ifcName fileIfcs)
 where
  replaceArrow str = subRegex (mkRegex ":=") str "<="

printModule elastic fileIfcs (Module name args ifcReverse ifcName ifcArgs provisos body) =
  "module " ++ name ++ args ++ "(" ++ ifcName ++ (if ifcReverse then "" else "_") ++ ifcArgs ++ ") " ++ provisos ++ ";\n" ++
  "  Tuple2#(" ++ ifcName ++ ifcArgs ++ ", " ++ ifcName ++ "_" ++ ifcArgs ++ ") mod" ++ (if ifcReverse then "_" else "") ++ " <- " ++ "_" ++ ifcName ++ ending ++
     (if ifcReverse then "  Tuple2#(" ++ ifcName ++ "_" ++ ifcArgs ++ ", " ++ ifcName ++ ifcArgs ++ ")" ++ " mod = tuple2(tpl_2(asIfc(mod_)), tpl1(asIfc(mod_)));\n" else "") ++
     modifyBody body ifcName fileIfcs ++
     (if elastic then "  rule _r;\n    if(tpl_2(asIfc(mod)).isSupplied)\n      (tpl_2(asIfc(mod))).hasBeenUsed;\n  endrule\n" else "") ++
  "  return tpl_2(asIfc(mod));\n" ++
  "endmodule\n\n"
 where
  ending = (if (ifcName == "Output" || ifcName == "Enable") then "(False, ?, True, True);\n" else ";\n")
