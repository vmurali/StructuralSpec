module PrintModule(printModule) where

import Text.Regex
import Data.Maybe
import Data.List

import DataTypes

getFields ifcName fileIfcs = (interfaceFields . fromJust) $ foldl (matchInterface ifcName) Nothing fileIfcs
 where
  matchInterface _       (Just found) _            = Just found
  matchInterface ifcName Nothing      (file, ifcs) = find (\x -> interfaceName x == ifcName) ifcs

prefixModule body field = subRegex (mkRegex (nonWordNonDot ++ field ++ nonWord)) body ("\\1(tpl_1(asIfc(mod_)))." ++ field ++ "\\2")
 where
  nonWordNonDot = "([^A-Za-z0-9_\\.]|^)"
  nonWord       = "([^A-Za-z0-9_]|$)"

modifyBody body ifcName fileIfcs = replaceArrow $ foldl prefixModule body (map (\x -> fieldName x) $ getFields ifcName fileIfcs)
 where
  replaceArrow str = subRegex (mkRegex ":=") str "<="

instancesDone body = concatMap showDone instanceLines
 where
  arrow = mkRegexWithOpts "[ \t\n]*<-[ \t\n]*" False True
  instanceLines = splitRegex arrow body
  spaces = mkRegexWithOpts "[ \t\n]+" False True
  showDone line = if getInstance line == [] then "" else "    (asIfc(" ++ getInstance line ++ ")).specCycleDone;\n"
  getInstance line = last $ splitRegex spaces line

printModule fileIfcs (Module name args ifcName ifcArgs provisos body) =
  "module " ++ name ++ args ++ "(" ++ ifcName ++ ifcArgs ++ ") " ++ provisos ++ ";\n" ++
  "  Tuple2#(" ++ ifcName ++ ifcArgs ++ ", " ++ ifcName ++ "_" ++ ifcArgs ++ ") mod_" ++ " <- " ++ "_" ++ ifcName ++ ending ++
     modifyBody body ifcName fileIfcs ++
  "  return tpl_2(asIfc(mod_));\n" ++
  "endmodule\n\n"
 where
  ending = (if (ifcName == "Output" || ifcName == "OutputPulse") then "(False, ?, True, True);\n" else ";\n")
