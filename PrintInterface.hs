module PrintInterface(printInterface) where

import Data.List

import DataTypes

printArgs numStr typeStr args = intercalate ", " $ map (printArg numStr typeStr) args
 where
  printArg numStr typeStr arg =
    case arg of
      Num x  -> numStr ++ x
      Type x -> typeStr ++ x

repLen field = replicate indicesLen
 where
  indicesLen = length $ fieldIndices field

concatRepLen field = concat . (repLen field)

showField field = "  interface " ++ repVectors field ++ ";\n"
 where
   showIndices = concatMap (\x -> "Vector#(" ++ x ++ ", ") $ fieldIndices field
   repVectors field = showIndices ++ fieldType field ++ fieldArgs field ++ (repLen field) ')' ++ " " ++ fieldName field

showRevField field = showField field {fieldType = "Rev" ++ fieldType field}

{-
interface = WrappedInput{Input x}
Module A which has WrappedInput interface reads mkInput. Thus, tpl_2(_).x is a read
Module B which instantiates the module which has WrappedInput interface writes to mkInput. Thus, tpl_1(_).x is a write

_ instantiates mkInput
tpl_2(_) = read of mkInput, tpl_1(_) = write of mkInput

interface = GuardedInput{Input x guard(y), InputPulse y}
Module A which has GuardedInput interface reads mkInput, mkInputPulse. Thus, tpl_2(_).x = read, with implicit guard for x

Module B which instantiates the module which has WrappedInput interface writes to mkInput. Thus, tpl_1(_).x = write . _ = mkInput


-}

printInterface (Interface name args fields) =
  "interface " ++ name ++ "#(" ++ printArgs "numeric type " "type " args ++ ");\n" ++
     concatMap showField fields ++
  "endinterface\n\n" ++
  "interface Rev" ++ name ++ "#(" ++ printArgs "numeric type" "type " args ++ ");\n" ++
     concatMap showRevField fields ++
  "endinterface\n\n" ++
  "typedef RevRev" ++ name ++ "#(" ++ printArgs "numeric type " "type " args ++ ") " ++ name ++ "#(" ++ printArgs "" "" args ++ ");\n\n" {-++
  "module _" ++ name ++ "#(Bool _guard1, Bool _guard2)(Tuple2#(" ++ name ++ args ++ ", Rev" ++ name ++ args ++ ")) provisos();\n" ++
     concatMap showFieldInst fields ++
  "  return(\n" ++
  "    interface " ++ name ++ args ++ " _i;\n" ++
         concatMap (showConn 1) fields ++
  "    endinterface,\n" ++
  "    interface Rev" ++ name ++ args ++ " i_;\n" ++
         concatMap (showConn 2) fields ++
  "    endinterface)\n" ++
  "endmodule\n\n" ++
  "module _Rev" ++ name ++ "#(Bool _guard2, Bool _guard1)(Tuple2#(Rev" ++ name ++ args ++ ", " ++ name ++ args ++ "));\n" ++
  "  Tuple2#(" ++ name ++ args ++ ", Rev" ++ name ++ args ++ ")" ++ " _this <- _" ++ name ++ "(_guard1, _guard);\n" ++
  "  return tuple2(tpl_2(_this), tpl_1(_this));\n" ++
  "endmodule\n\n"
  where

    generateGuard guards num = "_guard" ++ num ++ concatMap (\g -> " && (tpl_" ++ num ++ "(_" ++ g ++ "))._read") guards
    showFieldInst field = "  " ++ repDefs field ++ "_ <- " ++
                          concatRepLen field "replicateM(" ++ "_" ++ fieldName field ++ "(" ++ (generateGuard (fieldGuard field) "1") ++ ", " ++ (generateGuard (fieldGuardRev field) "2") ++ ")" ++ (repLen field) ')' ++ ";\n"

    showSimpleConn num field = "      interface " ++ fieldName field ++ " = tpl_" ++ show num ++ "(" ++ fieldName field ++ "_);\n"

    writeEns enables num = concatMap (\e -> "          (tpl_" ++ show num ++ "(_" ++ e ++ ")).send;\n") enables
    showEnConn enables num field = "      interface " ++ fieldType field ++ fieldName field ++ ";\n" ++
                                   "        method _write(x);\n" ++
                                   "          (tpl_" ++ show (3-num) ++ "(_" ++ fieldName field ++ "))._write(x);\n" ++
                                              writeEns enables (3-num) ++
                                   "        endmethod\n" ++
                                   "      endinterface\n"

    showConn num field = let enables = if num == 1 then fieldEn field else fieldEnRev field in
      case enables of
       [] -> showSimpleConn num field
       _  -> showEnConn enables num field
-}
