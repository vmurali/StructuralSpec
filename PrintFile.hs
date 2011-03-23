module PrintFile(printFile) where

import DataTypes
import PrintPartition
import PrintPort

printElement _         (Generic x) = x
printElement _         (Include x) = "import " ++ x ++ "::*;\n"
printElement _         x@(Port {}) = printPort x
printElement filePorts x@(Partition {}) = printPartition filePorts x

printFile ports elements =
  "import Vector::*;\n" ++
  "import HaskellLib::*;\n" ++
  "import Connectable::*;\n" ++
  "import Base::*;\n" ++
  "import Primitive::*;\n\n" ++
  (concatMap (printElement ports) elements)
