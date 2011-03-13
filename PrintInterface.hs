module PrintInterface(printInterface) where

import Data.List

import DataTypes

{-
for Input:

tpl_2 is the read
tpl_1 is the write

For guards in the normal direction:
  guards should be applied to implementation of tpl_2
  tpl_2 of the guard wires will be used for reading

For enables in the normal direction:
  the extra writes will be done in the implementation of tpl_2 for the main interface
  tpl_2 of the enable wires will be used for writing
-}

{-
TODO
1. Reverses : mkRev, mkRevRev
2. mkConnection between all this
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

printJustArgs = printArgs (\x-> "")

printProvisosArgs args = intercalate ", " provisos
 where
  provisos = ["Bits#(" ++ x ++ ", _sZ" ++ x ++ ")" | Type x <- args]
--------------------------------------------------------------------------

repVectors field = showIndices ++ fieldType field ++ fieldArgs field ++ (repLen field) ')' ++ " " ++ fieldName field
 where
  showIndices = concatMap (\x -> "Vector#(" ++ x ++ ", ") $ fieldIndices field

showField    field = "  interface " ++ repVectors field ++ ";\n"

showRevField field = showField field {fieldType = "Rev" ++ fieldType field}
-----------------------------------------------------------------------------------

showFieldInst field = "  " ++ repVectors field ++ "_ <- " ++
                      concatRepLen "replicateM(" ++ "_" ++ fieldType field ++ "(" ++ (genGuard (fieldGuardRev field) "1") ++ ", " ++ (genGuard (fieldGuard field) "2") ++ ")" ++ (repLen field) ')' ++ ";\n"
 where
  concatRepLen = concat . (repLen field)
  genGuard guards num = "_g" ++ num ++ concatMap (\g -> " && (tpl_" ++ num ++ "(" ++ g ++ "_))._read") guards
----------------------------------------------------------------------------------

showSimpleConn     num field = "      interface " ++ fieldName field ++ " = tpl_" ++ num ++ "(" ++ fieldName field ++ "_);\n"

showEnConn enables num field = "      interface " ++ fieldType field ++ fieldName field ++ ";\n" ++
                               "        method _write(x);\n" ++
                               "          (tpl_" ++ num ++ "(" ++ fieldName field ++ "_))._write(x);\n" ++
                                          concatMap (\e -> "          (tpl_" ++ num ++ "(" ++ e ++ "_)).send;\n") enables ++
                               "        endmethod\n" ++
                               "      endinterface\n"

showConn num field = let enables = if num == "1" then fieldEnRev field else fieldEnRev field in
  case enables of
   [] -> showSimpleConn num field
   _  -> showEnConn enables num field

----------------------------------------------------------------------------------

printInterface (Interface name args fields) =
  "interface " ++ name ++ "#(" ++ printKindArgs args ++ ");\n" ++
     concatMap showField fields ++
  "endinterface\n\n" ++
  "interface Rev" ++ name ++ "#(" ++ printKindArgs args ++ ");\n" ++
     concatMap showRevField fields ++
  "endinterface\n\n" ++
  "typedef " ++ name ++ "#(" ++ printJustArgs args ++ ") RevRev" ++ name ++ "#(" ++ printKindArgs args ++ ");\n" ++
  "module _" ++ name ++ "#(Bool _g1, Bool _g2)(Tuple2#(" ++ name ++ "#(" ++ printJustArgs args ++ "), Rev" ++ name ++ "#(" ++ printJustArgs args ++ "))) provisos(" ++ printProvisosArgs args ++ ");\n" ++
     concatMap showFieldInst fields ++
  "  return(\n" ++
  "    interface " ++ name ++ ";\n" ++
         concatMap (showConn "1") fields ++
  "    endinterface,\n" ++
  "    interface Rev" ++ name ++ ";\n" ++
         concatMap (showConn "2") fields ++
  "    endinterface)\n" ++
  "endmodule\n\n"
