module PrintPartition(printPartition) where

import Text.Regex
import Data.Maybe

import DataTypes
import SearchPort
import System.IO

nonWord       = "([^[:alnum:]_]|$)" --"([^A-Za-z0-9_]|$)"
nonWordNonDot = "([^[:alnum:]_\\.]|^)" --"([^A-Za-z0-9_\\.]|^)"

prefixPartition body field = subRegex (mkRegex (nonWordNonDot ++ field ++ nonWord)) body ("\\1(tpl_1(asIfc(mod_)))." ++ field ++ "\\2")

modifyBody body port = (replaceAtomic . replaceEndAtomic . replaceConnection . replaceArrow) $ foldl prefixPartition body (map (\x -> fieldName x) $ portFields port)
 where
  replaceArrow str = subRegex (mkRegexWithOpts "[ \n\t]*:=([^;]*);" False True) str ".write(\\1);"
  replaceAtomic str = subRegex (mkRegexWithOpts ("([ \t\n])atomic([ \t\n])") False True) str "\\1(* fire_when_enabled *) rule\\2"
  replaceEndAtomic str = subRegex (mkRegexWithOpts ("([ \t\n])endatomic([ \t\n])") False True) str "\\1endrule\\2"
  replaceConnection str = subRegex (mkRegexWithOpts "mk([[:alnum:]_]*)Connection[ \n\t]*\\(([^,]*),([^;]*)\\)" False True) str "mk\\1Connection(asIfc(\\2), asIfc(\\3))"

printPartition file filePortsAliases (Partition name args portNameLocal portArgs provisos body) = do
  actualPort <- if (portNameLocal == "Output" || portNameLocal == "OutputPulse" || portNameLocal == "ConditionalOutput" ||
                    portNameLocal == "OutputNormal" || portNameLocal == "OutputPulseNormal" || portNameLocal == "ConditionalOutputNormal")
                  then return $ Port{portName = portNameLocal, portFields = [], portArgs = undefined}
                  else case (searchPort portNameLocal filePortsAliases) of
                         Just x  -> return x
                         Nothing -> do
                           hPutStrLn stderr $ "Can not find Port " ++ portNameLocal ++ " in file " ++ file ++ ".spec"
                           return undefined
  let actualPortName = portName actualPort
  let ending = if (actualPortName == "Output" || actualPortName == "OutputPulse" || actualPortName == "ConditionalOutput" ||
                   actualPortName == "OutputNormal" || actualPortName == "OutputPulseNormal" || actualPortName == "ConditionalOutputNormal")
                 then "(True, True);\n"
                 else ";\n"
  return $
    "module " ++ name ++ args ++ "(" ++ portNameLocal ++ portArgs ++ ") " ++ provisos ++ ";\n" ++
    "  Tuple2#(" ++ portNameLocal ++ "_" ++ portArgs ++ ", " ++ portNameLocal ++ portArgs ++ ") mod_ <- _" ++ actualPortName ++ ending ++
       modifyBody body actualPort ++ "\n" ++
    "  return tpl_2(asIfc(mod_));\n" ++
    "endmodule\n\n"
