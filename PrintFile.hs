module PrintFile(printFile) where

import DataTypes
import PrintModule

{--
printInterface (Interface name args fields) =
  "interface " ++ name ++ "#" ++ args ++ ";\n" ++
     concatMap showField fields ++
  "endinterface\n\n" ++
  "interface Rev" ++ name ++ args ++ ";\n" ++
     concatMap showRevField fields ++
  "endinterface\n\n" ++
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
    showIndices field = concatMap (\x -> "Vector#(" ++ x ++ ", ") $ fieldIndices field
    repDefs field = showIndices field ++ fieldType field ++ fieldArgs field ++ (repLen field) ')' ++ " " ++ fieldName field
    showField field = "  interface " ++ repDefs field ++ ";\n"
    showRevField field = showField field {fieldType = "Rev" ++ fieldType field}

    indicesLen field = length $ fieldIndices field
    repLen field = replicate $ indicesLen field
    concatRepLen field = concat . (repLen field)

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
--}

printElement fileIfcs (Generic x) = x
printElement fileIfcs (Import x) = "import " ++ x ++ "::*;\n"
--printElement fileIfcs x@(Interface {}) = printInterface x
printElement fileIfcs x@(Module {}) = printModule fileIfcs x

printFile elements ifcs =
  "import Vector::*;\n" ++
  "import Library::*;\n\n" ++
  (concatMap (printElement ifcs) elements)
