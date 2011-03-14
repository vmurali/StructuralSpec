module PrintInterface(printInterface) where

import Data.List

import DataTypes

{-
for Input:

tpl_1 is the read
tpl_2 is the write

For guard in the normal direction:
  guards should be applied to implementation of tpl_1
  tpl_1 of the guard wires will be used for reading

For enable in the normal direction:
  the extra writes will be done in the implementation of tpl_1 for the main interface
  tpl_1 of the enable wires will be used for writing
-}

-----------------------------------------------------------------------

repLen field = replicate indicesLen
 where
  indicesLen = length $ fieldIndices field
-------------------------------------------------------------------------

printArgs printFn args = intercalate ", " $ map printFn args

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

printProvisosArgs args = intercalate ", " provisos
 where
  provisos = ["Bits#(" ++ x ++ ", _sZ" ++ x ++ ")" | Type x <- args]
--------------------------------------------------------------------------

repVectors field = showIndices ++ fieldType field ++ "#" ++ fieldArgs field ++ (repLen field) ')'
 where
  showIndices = concatMap (\x -> "Vector#(" ++ x ++ ", ") $ fieldIndices field

showField field =
  "  interface " ++ repVectors field ++ " " ++ fieldName field ++ ";\n" ++
  if fieldDefault field
    then if (not $ fieldReverse field) == (fieldType field == "Input")
           then "method " ++ fieldArgs field ++ " _read();\n"
           else "method Action _write(" ++ fieldArgs field ++ " x);\n"
    else ""

showRevField field = showField field {fieldType = fieldType field ++ "_", fieldReverse = not $ fieldReverse field}
-----------------------------------------------------------------------------------

showFieldInst field = "  " ++ typeTuple ++ fieldName field ++ ubarForRev ++ "_ <- " ++ concatRepLen "replicateM(" ++ "_" ++ fieldType field ++ params ++ (repLen field) ')' ++ ";\n" ++
                      if rev then "  " ++ typeTupleRev ++ fieldName field ++ "_ = tuple2(tpl_2(" ++ fieldName field ++ "__), tpl_1(" ++ fieldName field ++ "__));\n" else ""
 where
  concatRepLen = concat . (repLen field)
  typeTuple    = "Tuple2#(" ++ repVectors field ++ ", " ++ repVectors field{fieldType = fieldType field ++ "_"} ++ ") "
  typeTupleRev = "Tuple2#(" ++ repVectors field{fieldType = fieldType field ++ "_"} ++ ", " ++ repVectors field ++ ") "
  rev = fieldReverse field
  ubarForRev = if rev then "_" else ""
  en    = fieldEn    field
  enRev = fieldEnRev field
  enValid = en /= []
  enRevValid = enRev /= []
  tpl1En = if enValid then "(tpl_1(" ++ en ++ "_))._read" else "?"
  tpl2EnRev = if enRevValid then "(tpl_2(" ++ enRev ++ "_))._read" else "?"
  g    = fieldGuard field
  gRev = fieldGuardRev field
  gValid = g /= []
  gRevValid = gRev /= []
  tpl1G = if gValid then "(tpl_1(" ++ g ++ "_))._read && _g1" else "_g1"
  tpl2GRev = if gRevValid then "(tpl_2(" ++ gRev ++ "_))._read && _g2" else "_g2"
  params =
    if rev
      then "(" ++ show enRevValid ++ ", " ++ tpl2EnRev ++ ", " ++ show enValid ++ ", " ++ tpl1En ++ ", " ++ tpl2GRev ++ ", " ++ tpl1G ++ ")"
      else "(" ++ show enValid ++ ", " ++ tpl1En ++ ", " ++ show enRevValid ++ ", " ++ tpl2EnRev ++ ", " ++ tpl1G ++ ", " ++ tpl2GRev ++ ")"
  
----------------------------------------------------------------------------------

showConn num field =
  if fieldType field == "Enable"
    then if (num == "1" && not (fieldReverse field)) || (num == "2" && fieldReverse field)
           then "      method Action " ++ fieldName field ++ " = (tpl_" ++ num ++ "(" ++ fieldName field ++ "_)).send;\n"
           else showNormalConn
    else showNormalConn ++
         if fieldDefault field
           then if (num == "1") ==  ((not $ fieldReverse field) == (fieldType field == "Input"))
                  then showDefaultRead
                  else showDefaultWrite
           else ""
 where
  showNormalConn = "      interface " ++ fieldName field ++ " = tpl_" ++ num ++ "(" ++ fieldName field ++ "_);\n"
  showDefaultWrite = "      method _write = (tpl_" ++ num ++ "(" ++ fieldName field ++ "_))._write;\n"
  showDefaultRead  = "      method  _read = (tpl_" ++ num ++ "(" ++ fieldName field ++ "_))._read;\n"

----------------------------------------------------------------------------------

printInterface (Interface name args fields) =
  "interface " ++ name ++ "#(" ++ printKindArgs args ++ ");\n" ++
     concatMap showField fields ++
  "endinterface\n\n" ++
  "interface " ++ name ++ "_#(" ++ printKindArgs args ++ ");\n" ++
     concatMap showRevField fields ++
  "endinterface\n\n" ++
  "module _" ++ name ++ "#(Bool _en1Valid, Enable _en1, Bool _en2Valid, Enable _en2, Bool _g1, Bool _g2)" ++
                         "(Tuple2#(" ++ name ++ "#(" ++ printJustArgs args ++ "), " ++ name ++ "_#(" ++ printJustArgs args ++ "))) provisos(" ++ printProvisosArgs args ++ ");\n" ++
     concatMap showFieldInst fields ++
  "  return tuple2(\n" ++
  "    interface " ++ name ++ ";\n" ++
         concatMap (showConn "1") fields ++
  "    endinterface,\n" ++
  "    interface " ++ name ++ "_;\n" ++
         concatMap (showConn "2") fields ++
  "    endinterface);\n" ++
  "endmodule\n\n" ++
  "instance Connectable#(" ++ name ++ "#(" ++ printJustArgs args ++ "), " ++ name ++ "_#(" ++ printJustArgs args ++ "));\n" ++
  "  module mkConnection#(" ++ name ++ "#(" ++ printJustArgs args ++ ") a, " ++ name ++ "_#(" ++ printJustArgs args ++ ") b)();\n" ++
       concatMap (\x -> "    mkConnection(a." ++ x ++ ", b." ++ x ++ ");\n") (map fieldName fields) ++
  "  endmodule\n" ++
  "endinstance\n\n" ++
  "instance Connectable#(" ++ name ++ "_#(" ++ printJustArgs args ++ "), " ++ name ++ "#(" ++ printJustArgs args ++ "));\n" ++
  "  module mkConnection#(" ++ name ++ "_#(" ++ printJustArgs args ++ ") a, " ++ name ++ "#(" ++ printJustArgs args ++ ") b)();\n" ++
       concatMap (\x -> "    mkConnection(a." ++ x ++ ", b." ++ x ++ ");\n") (map fieldName fields) ++
  "  endmodule\n" ++
  "endinstance\n\n"
