module PrintSpec(printFile) where

import DataTypes
import Text.Regex
import Data.Maybe
import Data.List

printInterface (Interface name args fields) =
  "interface " ++ name ++ args ++ ";\n" ++
     concatMap showField fields ++
  "endinterface\n\n" ++
  "interface Rev" ++ name ++ args ++ ";\n" ++
     concatMap showRevField fields ++
  "endinterface\n\n" ++
  "module _" ++ name ++ "(Tuple2#(" ++ name ++ args ++ ", Rev" ++ name ++ args ++ "));\n" ++
     concatMap showFieldInst fields ++
  "  return(\n" ++
  "    interface " ++ name ++ args ++ " _i;\n" ++
         concatMap (showConn "1") fields ++
  "    endinterface,\n" ++
  "    interface Rev" ++ name ++ args ++ " i_;\n" ++
         concatMap (showConn "2") fields ++
  "    endinterface)\n" ++
  "endmodule\n\n" ++
  "module _Rev" ++ name ++ "(Tuple2#(Rev" ++ name ++ args ++ ", " ++ name ++ args ++ "));\n" ++
  "  Tuple2#(" ++ name ++ args ++ ", Rev" ++ name ++ args ++ ")" ++ " this <- _" ++ name ++ ";\n" ++
  "  return tuple2(tpl_2(this), tpl_1(this));\n" ++
  "endmodule\n\n"
  where
    showIndices field = concatMap (\x -> "Vector#(" ++ x ++ ", ") $ fieldIndices field
    repDefs field = showIndices field ++ fieldType field ++ fieldArgs field ++ (repLen field) ')' ++ " " ++ fieldName field
    showField field = "  interface " ++ repDefs field ++ ";\n"
    showRevField field = showField field {fieldType = "Rev" ++ fieldType field}
    indicesLen field = length $ fieldIndices field
    repLen field = replicate $ indicesLen field
    concatRepLen field = concat . (repLen field)
    showFieldInst field = "  " ++ repDefs field ++ "_ <- " ++ concatRepLen field "replicateM(" ++ "_" ++ fieldName field ++ (repLen field) ')' ++ ";\n"
    showConn num field = "      interface " ++ fieldName field ++ " = tpl_" ++ num ++ "(" ++ fieldName field ++ "_);\n"

printModule (Module name args ifcName ifcArgs provisos body) ifcFields =
  "module " ++ name ++ args ++ "(" ++ ifcName ++ ifcArgs ++ ") " ++ provisos ++ ";\n" ++
  "  Tuple2#(" ++ ifcName ++ args ++ "," ++ ifcName ++ "_" ++ args ++ ")" ++ "_this <- " ++ "_" ++ ifcName ++ ";\n" ++
     newBody ++
  "  return tpl_1(_this);\n" ++
  "endmodule\n\n"
  where
    nonWordDot = "([^A-Za-z0-9_\\.]|^)"
    nonWord = "([^A-Za-z0-9_]|$)"
    createSub inp x = subRegex (mkRegex (nonWordDot ++ x ++ nonWord)) inp ("\\1(tpl_2(_this))." ++ x ++ "\\2")
    replaceArrow str = subRegex (mkRegexWithOpts ":=" True True) str "<="
    newBody = replaceArrow $ foldl createSub body (map (\x -> fieldName x) ifcFields)

printElement ifcs (Generic x) = x
printElement ifcs (Import x) = "import " ++ x ++ "::*;\n"
printElement ifcs x@(Interface {}) = printInterface x
printElement ifcs x@(Module {implementName=ifcName}) = printModule x (findInterface ifcs ifcName)
  where
    getInterface ifcName (Just ifc) (file, ifcs) = Just ifc
    getInterface ifcName Nothing    (file, ifcs) = find (\x -> interfaceName x == ifcName) ifcs
    findInterface allIfcs ifcName = (interfaceFields . fromJust) $ foldl (getInterface ifcName) Nothing allIfcs

printFile elements ifcs =
  "import Vector::*;\nimport Library::*;\n\n" ++
  (concatMap (printElement ifcs) elements)
