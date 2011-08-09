module PrintFile(printFile) where

import DataTypes
import PrintPort
import PrintInstance
import PrintPartition
import PrintAlias

printElement _    _                (Generic x) = return x
printElement _    _                (Include x) = return $ "import " ++ x ++ "::*;\nexport " ++ x ++ "::*;\n\n"
printElement _    _                x@(Alias {}) = printAlias x
printElement file filePortsAliases x@(Port {}) = printPort file filePortsAliases x
printElement _    _                x@(Instance {}) = printInstance x
printElement file filePortsAliases x@(Partition {}) = printPartition file filePortsAliases x

printFile file portsAliases elements = do
  full <- foldl (\m1 elem -> do{x <- m1; y <- printElement file portsAliases elem; return $ x ++ y}) (return []) elements
  return $
    "import Vector::*;\n" ++
    "import HaskellLib::*;\n" ++
    "import Connectable::*;\n" ++
    "import Base::*;\n" ++
    "import Primitive::*;\n" ++
    "export " ++ file ++ "::*;\n\n" ++
    full

