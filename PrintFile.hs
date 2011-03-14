module PrintFile(printFile) where

import DataTypes
import PrintModule
import PrintInterface

printElement fileIfcs (Generic x) = x
printElement fileIfcs (Import x) = "import " ++ x ++ "::*;\n"
printElement fileIfcs x@(Interface {}) = printInterface x
printElement fileIfcs x@(Module {}) = printModule fileIfcs x

printFile elements ifcs =
  "import Vector::*;\n" ++
  "import ReplicateTuple::*;\n" ++
  "import Connectable::*;\n" ++
  "import Primitive::*;\n\n" ++
  (concatMap (printElement ifcs) elements)
