module Advita.Model.Module where

import           GHC.Generics           (Generic)

import           Perst.Database.DataDef (DataD, DelCons (..), TableD)

import           Advita.Model.Fields    (Ident, VarChar)

data W_module = W_module
  { id          :: Ident W_module
  , _class      :: VarChar 50
  , name        :: VarChar 50
  , description :: VarChar 255
  } deriving (Show, Eq, Generic)

type TModuleTab = TableD W_module '["id"] '[ '["class"]] True

type TModule = DataD TModuleTab '[]

type ModuleRef (x :: DelCons) = '(TModuleTab, '[ '("id_module","id")], x)
