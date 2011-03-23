module PrintFile(printFile) where

import DataTypes
import PrintModule
import PrintInterface

printElement _        (Generic x) = x
printElement _        (Import x) = "import " ++ x ++ "::*;\n"
printElement _        x@(Interface {}) = printInterface x
printElement fileIfcs x@(Module {}) = printModule fileIfcs x

printFile elastic ifcs elements =
  "import Vector::*;\n" ++
  "import HaskellLib::*;\n" ++
  "import Connectable::*;\n" ++
  "import Primitive" ++ (if elastic then "Elastic" else "") ++ "::*;\n\n" ++
  (concatMap (printElement ifcs) elements)
