module PrintPort(printPort) where

import Data.List
import Data.Maybe

import DataTypes
import SearchPort

import System.IO

-----------------------------------------------------------------------

repLen field = replicate indicesLen
 where
  indicesLen = length $ fieldIndices field
-------------------------------------------------------------------------

printArgs printFn args = if null args then "" else "#(" ++ (intercalate ", " $ map printFn args) ++ ")"

printKindArgs = printArgs printKind
 where
  printKind arg =
    case arg of
      Num  x -> "numeric type " ++ x
      Type x -> "type " ++ x

printJustArgs = printArgs printNone
 where
  printNone arg =
    case arg of
      Num  x -> x
      Type x -> x

printProvisosArgs args = if null args then "" else "provisos(" ++ (intercalate ", " provisos) ++ ")"
 where
  provisos = ["Bits#(" ++ x ++ ", _sZ" ++ x ++ ")" | Type x <- args]
---------------------------------------

ubarForNormal field = if fieldReverse field then "" else "_"
------------------------------------------------------------------------

repVectors field = showIndices ++ fieldType field ++ ubarForNormal field ++ (if fieldArgs field /= [] then "#" else "") ++ fieldArgs field ++ (repLen field) ')'
 where
  showIndices = concatMap (\x -> "Vector#(" ++ x ++ ", ") $ fieldIndices field

showField field = "  interface " ++ repVectors field ++ " " ++ fieldName field ++ ";\n"

showRevField field = showField field {fieldReverse = not $ fieldReverse field}
-----------------------------------------------------------------------------------

showFieldInst fileName p filePortsAliases field = do
  fieldPortName <- if (fieldType field == "Output" || fieldType field == "OutputPulse" || fieldType field == "ConditionalOutput" ||
                       fieldType field == "OutputNormal" || fieldType field == "OutputPulseNormal" || fieldType field == "ConditionalOutputNormal")
                      then return $ fieldType field
                      else case (searchPort (fieldType field) filePortsAliases) of
                             Just x  -> return $ portName x
                             Nothing -> do
                               hPutStrLn stderr $ "Can not find Field " ++ fieldType field ++ " of Port " ++ p ++ " in File " ++ fileName ++ ".spec"
                               return undefined
  let params = if (fieldPortName == "Output" || fieldPortName == "OutputPulse" || fieldPortName == "ConditionalOutput" ||
                   fieldPortName == "OutputNormal" || fieldPortName == "OutputPulseNormal" || fieldPortName == "ConditionalOutputNormal")
                 then "(" ++ g1 ++ ", " ++ g2 ++ ")"
                 else ""
  return $
    "  " ++ typeTuple ++ fieldName field ++ "_ <- " ++ concatRepLen "replicateTupleM(" ++ "_" ++ fieldPortName ++ params ++ (repLen field) ')' ++ ";\n"
 where
  concatRepLen = concat . (repLen field)
  typeTupleNormal   = "Tuple2#(" ++ repVectors field ++ ", " ++ repVectors field{fieldReverse = not $ fieldReverse field} ++ ") "
  typeTupleReversed = "Tuple2#(" ++ repVectors field{fieldReverse = not $ fieldReverse field} ++ ", " ++ repVectors field ++ ") "
  typeTuple = if fieldReverse field then typeTupleReversed else typeTupleNormal
  g1 = if fieldWriteGuard field /= ""
         then "(tpl_2(asIfc(" ++ fieldWriteGuard field ++ "_)))._read"
         else "True"
  g2 = if fieldReadGuard field /= ""
         then "(tpl_2(asIfc(" ++ fieldReadGuard field ++ "_)))._read"
         else "True"
----------------------------------------------------------------------------------
showConn num field = "      interface " ++ fieldName field ++ " = tpl_" ++ realNum ++ "(asIfc(" ++ fieldName field ++ "_));\n"
 where
  realNum = if fieldReverse field then show (3-num) else show num
---------------------------------------------------------------------------------
mkConnection field = "    mkConnection(asIfc(a." ++ fieldName field ++ "), asIfc(b." ++ fieldName field ++ "));\n"
----------------------------------------------------------------------------------
printPort file filePortsAliases (Port name args oldFields) = do
  fieldInsts <- foldl (\m1 field -> do{x <- m1; y <- showFieldInst file name filePortsAliases field; return $ x ++ y}) (return []) fields
  return $
   "interface " ++ name ++ "_" ++ printKindArgs args ++ ";\n" ++
      concatMap showField fields ++
   "endinterface\n\n" ++
   "interface " ++ name ++ printKindArgs args ++ ";\n" ++
      concatMap showRevField fields ++
   "endinterface\n\n" ++
   "module _" ++ name ++ "(Tuple2#(" ++ name ++ "_" ++ printJustArgs args ++ ", " ++ name ++ printJustArgs args ++ ")) " ++ printProvisosArgs args ++ ";\n" ++
      fieldInsts ++
   "  return tuple2(\n" ++
   "    interface " ++ name ++ "_;\n" ++
          concatMap (showConn 1) fields ++
   "    endinterface,\n" ++
   "    interface " ++ name ++ ";\n" ++
          concatMap (showConn 2) fields ++
   "    endinterface);\n" ++
   "endmodule\n\n" ++
       "instance Connectable#(" ++ name ++ printJustArgs args ++ ", " ++ name ++ "_" ++ printJustArgs args ++ ") " ++ printProvisosArgs args ++ ";\n" ++
       "  module mkConnection#(" ++ name ++ printJustArgs args ++ " a, " ++ name ++ "_" ++ printJustArgs args ++ " b)();\n" ++
            concatMap mkConnection fields ++
       "  endmodule\n" ++
       "endinstance\n\n" ++
       "instance Connectable#(" ++ name ++ "_" ++ printJustArgs args ++ ", " ++ name ++ printJustArgs args ++ ") " ++ printProvisosArgs args ++ ";\n" ++
       "  module mkConnection#(" ++ name ++ "_" ++ printJustArgs args ++ " a, " ++ name ++ printJustArgs args ++ " b)();\n" ++
       "    mkConnection(asIfc(b), asIfc(a));\n" ++
       "  endmodule\n" ++
       "endinstance\n\n"
  where
   fields = map (removeConditionalInputNormal . removeInputPulseNormal . removeInputNormal . removeConditionalInput . removeInputPulse . removeInput) oldFields
    where
     removeInput field = if fieldType field == "Input" then field{fieldType = "Output", fieldReverse = not $ fieldReverse field} else field
     removeInputPulse field = if fieldType field == "InputPulse" then field{fieldType = "OutputPulse", fieldReverse = not $ fieldReverse field} else field
     removeConditionalInput field = if fieldType field == "ConditionalInput" then field{fieldType = "ConditionalOutput", fieldReverse = not $ fieldReverse field} else field
     removeInputNormal field = if fieldType field == "InputNormal" then field{fieldType = "OutputNormal", fieldReverse = not $ fieldReverse field} else field
     removeInputPulseNormal field = if fieldType field == "InputPulseNormal" then field{fieldType = "OutputPulseNormal", fieldReverse = not $ fieldReverse field} else field
     removeConditionalInputNormal field = if fieldType field == "ConditionalInputNormal" then field{fieldType = "ConditionalOutputNormal", fieldReverse = not $ fieldReverse field} else field
