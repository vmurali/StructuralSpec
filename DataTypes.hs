module DataTypes where

data Field = Field
  { fieldDefault::Bool
  , fieldReverse::Bool
  , fieldType::String
  , fieldArgs::String
  , fieldIndices::[String]
  , fieldName::String
  , fieldEn::String
  , fieldEnRev::String
  , fieldGuard::String
  , fieldGuardRev::String
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
  | Include String
  | Generic String
