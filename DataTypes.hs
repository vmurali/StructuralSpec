module DataTypes where

data Default = Read | Write | None deriving Eq

data Field = Field
  { fieldReverse::Bool
  , fieldDefault::Default
  , fieldType::String
  , fieldArgs::String
  , fieldIndices::[String]
  , fieldName::String
  , fieldEn::String
  , fieldEnRev::String
  , fieldGuard::String
  , fieldGuardRev::String
  }

data InterfaceArg = Type String | Num String

data Element = Interface
    { interfaceName::String
    , interfaceArgs::[InterfaceArg]
    , interfaceFields::[Field]
    }
  | Module
    { moduleName::String
    , moduleArgs::String
    , implementName::String
    , implementArgs::String
    , moduleProvisos::String
    , moduleBody::String
    }
  | Import String
  | Generic String
