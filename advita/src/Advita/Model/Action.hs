module Advita.Model.Action where

import           GHC.Generics           (Generic)

import           Perst.Database.DataDef (DataD, DelCons (..), TableD)

import           Advita.Model.Fields    (Ident, VarChar)
import           Advita.Model.Module    (ModuleRef, W_module)

data W_action = W_action
  { id          :: Ident W_action
  , id_module   :: Ident W_module
  , name        :: VarChar 50
  , description :: VarChar 255
  } deriving (Show, Eq, Generic)

type TActionTab = TableD W_action '["id"] '[] True

type TAction = DataD TActionTab '[ModuleRef DcRestrict]

type ActionRef (x :: DelCons) = '(TActionTab, '[ '("id_action", "id")], x)
