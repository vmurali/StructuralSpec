module PrintFile(printFile) where

import DataTypes
import PrintModule
import PrintInterface

printElement _       _        (Generic x) = x
printElement _       _        (Import x) = "import " ++ x ++ "::*;\n"
printElement elastic _        x@(Interface {}) = printInterface elastic x
printElement elastic fileIfcs x@(Module {}) = printModule elastic fileIfcs x

printFile elastic ifcs elements =
  "import Vector::*;\n" ++
  "import ReplicateTuple::*;\n" ++
  "import Connectable::*;\n" ++
  "import Primitive::*;\n\n" ++
  (concatMap (printElement elastic ifcs) elements)
