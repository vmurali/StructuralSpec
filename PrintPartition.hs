module PrintPartition(printPartition) where

import Text.Regex
import Data.Maybe
import Data.List

import DataTypes

getFields implementName filePorts = (portFields . fromJust) $ foldl matchPort Nothing filePorts
 where
  matchPort (Just found) _            = Just found
  matchPort Nothing      (file, ports) = find (\x -> portName x == implementName) ports

nonWord       = "([^A-Za-z0-9_]|$)"
nonWordNonDot = "([^A-Za-z0-9_\\.]|^)"

prefixPartition body field = subRegex (mkRegex (nonWordNonDot ++ field ++ nonWord)) body ("\\1(tpl_1(asIfc(mod_)))." ++ field ++ "\\2")
 where

modifyBody body portName filePorts = (replaceDone . replaceArrow) $ foldl prefixPartition body (map (\x -> fieldName x) $ getFields portName filePorts)
 where
  replaceArrow str = subRegex (mkRegex ":=") str "<="
  replaceDone str = subRegex (mkRegex (nonWordNonDot ++ "specCycleDone" ++ nonWord)) str ("\\1(tpl_1(asIfc(mod_))).specCycleDone" ++ "\\2")

instancesDone body = concatMap showDone instanceLines
 where
  arrow = mkRegexWithOpts "[ \t\n]*<-[ \t\n]*" False True
  instanceLines = splitRegex arrow body
  spaces = mkRegexWithOpts "[ \t\n]+" False True
  showDone line = if getInstance line == [] then "" else "    (asIfc(" ++ getInstance line ++ ")).specCycleDone;\n"
  getInstance line = last $ splitRegex spaces line

printPartition filePorts (Partition name args portName portArgs provisos body) =
  "module " ++ name ++ args ++ "(" ++ portName ++ "_" ++ portArgs ++ ") " ++ provisos ++ ";\n" ++
  "  Tuple2#(" ++ portName ++ portArgs ++ ", " ++ portName ++ "_" ++ portArgs ++ ") mod_" ++ " <- " ++ "_" ++ portName ++ ending ++
     modifyBody body portName filePorts ++ "\n" ++
  "  return tpl_2(asIfc(mod_));\n" ++
  "endmodule\n\n"
 where
  ending = (if (portName == "Output" || portName == "OutputPulse") then "(False, ?, True, True);\n" else ";\n")