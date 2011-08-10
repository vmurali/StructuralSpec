module DataTypes where

data Field = Field
  { fieldReverse::Bool
  , fieldType::String
  , fieldArgs::String
  , fieldIndices::[String]
  , fieldName::String
  , fieldWriteGuard::String
  , fieldReadGuard::String
  }

data PortArg = Type String | Num String

data Element = Port
    { portName::String
    , portArgs::[PortArg]
    , portFields::[Field]
    }
  | Partition
    { partitionName::String
    , partitionArgs::String
    , implementName::String
    , implementArgs::String
    , partitionProvisos::String
    , partitionBody::String
    }
  | Instance
    { instanceName::String
    , instanceArgs::String
    , instanceImplName::String
    , instanceImplArgs::String
    , instanceProvisos::String
    , instanceExpr::String
    }
  | Alias
    { aliasName::String
    , aliasParams::String
    , aliasPort::String
    , aliasPortParams::String
    }
  | Include String
  | Generic String
