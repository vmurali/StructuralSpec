module DataTypes where

data Field =
  Field
  { fieldType::String
  , fieldArgs::String
  , fieldIndices::[String]
  , fieldName::String
  , fieldEn1::[String]
  , fieldEn2::[String]
  , fieldGuard1::[String]
  , fieldGuard2::[String]
  } deriving Show

data Element =
    Interface
    { interfaceName::String
    , interfaceArgs::String
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
  | Generic Char
