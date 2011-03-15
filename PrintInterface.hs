module PrintInterface(printInterface) where

import Data.List

import DataTypes

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

ubarForRev field = if fieldReverse field then "_" else ""
------------------------------------------------------------------------

repVectors field = showIndices ++ fieldType field ++ ubarForRev field ++ (if fieldArgs field /= [] then "#" else "") ++ fieldArgs field ++ (repLen field) ')'
 where
  showIndices = concatMap (\x -> "Vector#(" ++ x ++ ", ") $ fieldIndices field

showField field =
  "  interface " ++ repVectors field ++ " " ++ fieldName field ++ ";\n" ++
  if fieldDefault field
    then
      if not $ fieldReverse field
        then "  method Action _write(" ++ fieldArgs field ++ " x);\n"
        else "  method " ++ fieldArgs field ++ " _read();\n"
    else ""

showRevField field = showField field {fieldReverse = not $ fieldReverse field}
-----------------------------------------------------------------------------------

showFieldInst field = "  " ++ typeTuple ++ fieldName field ++ ubarForRev field ++ "_ <- " ++ concatRepLen "replicateTupleM(" ++ "_" ++ fieldType field ++ params ++ (repLen field) ')' ++ ";\n" ++
                      if rev then "  " ++ typeTupleRev ++ fieldName field ++ "_ = tuple2(tpl_2(asIfc(" ++ fieldName field ++ "__)), tpl_1(asIfc(" ++ fieldName field ++ "__)));\n" else ""
 where
  concatRepLen = concat . (repLen field)
  typeTupleNormal   = "Tuple2#(" ++ repVectors field ++ ", " ++ repVectors field{fieldReverse = not $ fieldReverse field} ++ ") "
  typeTupleReversed = "Tuple2#(" ++ repVectors field{fieldReverse = not $ fieldReverse field} ++ ", " ++ repVectors field ++ ") "
  typeTuple = if rev then typeTupleReversed else typeTupleNormal
  typeTupleRev = if rev then typeTupleNormal else typeTupleReversed
  rev = fieldReverse field
  enStr = if fieldEn field /= []
            then "True, tpl_1(asIfc(" ++ fieldEn field ++ "_))"
            else
              if fieldEnRev field /= []
                then "True, tpl_2(asIfc(" ++ fieldEnRev field ++ "_))"
                else "False, ?"
  g1 = if fieldGuard field /= []
         then "(tpl_1(asIfc(" ++ fieldGuard field ++ "_)))._read"
         else "True"
  g2 = if fieldGuardRev field /= []
         then "(tpl_2(asIfc(" ++ fieldGuardRev field ++ "_)))._read"
         else "True"
  params = if (fieldType field == "Output" || fieldType field == "Enable")
             then "(" ++ enStr ++ ", " ++
               (if rev
                  then g2 ++ ", " ++ g1
                  else g1 ++ ", " ++ g2)
               ++ ")"
             else ""
  
----------------------------------------------------------------------------------

showConn num field =
  "      interface " ++ fieldName field ++ " = tpl_" ++ num ++ "(asIfc(" ++ fieldName field ++ "_));\n" ++
  if fieldDefault field
    then
      if (fieldReverse field) /= (num == "1")
        then assign "_write"
        else assign "_read"
    else ""
 where
  assign str = "      method " ++ str ++ " = (tpl_" ++ num ++ "(asIfc(" ++ fieldName field ++ "_)))." ++ str ++ ";\n"

----------------------------------------------------------------------------------
extrasField elastic = if elastic then "  method Action hasBeenUsed();\n  method Bool isSupplied();\n" else ""

extrasMethods elastic num fields = 
  if elastic
    then "      method Action hasBeenUsed();\n" ++
                  concatMap (\x -> "        _hasBeenUsed(tpl_" ++ num ++ "(asIfc(" ++ x ++ "_)));\n") (map fieldName fields) ++
         "      endmethod\n" ++
         "      method Bool isSupplied = " ++ intercalate " && " (map ((\x -> "_isSupplied(tpl_" ++ num ++ "(asIfc(" ++ x ++ "_)))") . fieldName) fields) ++ ";\n"
    else ""

extrasInstances elastic name args =
  if elastic
    then "instance Sync_#(" ++ name ++ printJustArgs args ++ ");\n" ++
         "  function Action _hasBeenUsed(" ++ name ++ printJustArgs args ++ " x) = x.hasBeenUsed;\n" ++
         "  function Bool _isSupplied(" ++ name ++ printJustArgs args ++ " x) = x.isSupplied;\n" ++
         "endinstance\n\n" ++
         "instance Sync_#(" ++ name ++ "_" ++ printJustArgs args ++ ");\n" ++
         "  function Action _hasBeenUsed(" ++ name ++ "_" ++ printJustArgs args ++ " x) = x.hasBeenUsed;\n" ++
         "  function Bool _isSupplied(" ++ name ++ "_" ++ printJustArgs args ++ " x) = x.isSupplied;\n" ++
         "endinstance\n\n"
    else ""
------------------------------------------------------------------------------------------------------
printInterface elastic (Interface name args oldFields) =
  "interface " ++ name ++ printKindArgs args ++ ";\n" ++
     concatMap showField fields ++
     extrasField elastic ++
  "endinterface\n\n" ++
  "interface " ++ name ++ "_" ++ printKindArgs args ++ ";\n" ++
     concatMap showRevField fields ++
     extrasField elastic ++
  "endinterface\n\n" ++
  "module _" ++ name ++ "(Tuple2#(" ++ name ++ printJustArgs args ++ ", " ++ name ++ "_" ++ printJustArgs args ++ ")) " ++ printProvisosArgs args ++ ";\n" ++
     concatMap showFieldInst fields ++
  "  return tuple2(\n" ++
  "    interface " ++ name ++ ";\n" ++
         concatMap (showConn "1") fields ++
         extrasMethods elastic "1" fields ++
  "    endinterface,\n" ++
  "    interface " ++ name ++ "_;\n" ++
         concatMap (showConn "2") fields ++
         extrasMethods elastic "2" fields ++
  "    endinterface);\n" ++
  "endmodule\n\n" ++
  "instance Connectable#(" ++ name ++ printJustArgs args ++ ", " ++ name ++ "_" ++ printJustArgs args ++ ") " ++ printProvisosArgs args ++ ";\n" ++
  "  module mkConnection#(" ++ name ++ printJustArgs args ++ " a, " ++ name ++ "_" ++ printJustArgs args ++ " b)();\n" ++
       concatMap (\x -> "    mkConnection(asIfc(a." ++ x ++ "), asIfc(b." ++ x ++ "));\n") (map fieldName fields) ++
  "  endmodule\n" ++
  "endinstance\n\n" ++
  "instance Connectable#(" ++ name ++ "_" ++ printJustArgs args ++ ", " ++ name ++ printJustArgs args ++ ") " ++ printProvisosArgs args ++ ";\n" ++
  "  module mkConnection#(" ++ name ++ "_" ++ printJustArgs args ++ " a, " ++ name ++ printJustArgs args ++ " b)();\n" ++
       concatMap (\x -> "    mkConnection(asIfc(a." ++ x ++ "), asIfc(b." ++ x ++ "));\n") (map fieldName fields) ++
  "  endmodule\n" ++
  "endinstance\n\n" ++
  extrasInstances elastic name args
 where
  fields = map removeInput oldFields
   where
    removeInput field = if fieldType field == "Input" then field{fieldType = "Output", fieldReverse = not $ fieldReverse field} else field
