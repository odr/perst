module Advita.Model.Access where

import           GHC.Generics           (Generic)

import           Data.Type.Grec         (GrecGroup)
import           Perst.Database.DataDef (DataD, DelCons (..), TableD)

import           Advita.Model.Action    (ActionRef, W_action)
import           Advita.Model.Fields    (Ident, SizedInt, VarChar)
import           Advita.Model.Module    (ModuleRef, W_module)
import           Advita.Model.Role      (RoleRef, U_role)
import           Advita.Model.State     (StateRef, W_state)
import           Advita.Model.User      (Stamp, StampFK)

data W_access = W_access
  { id        :: Ident W_access
  , id_module :: Ident W_module
  , id_action :: Ident W_action
  , id_role   :: Ident U_role
  , id_state  :: Maybe (Ident W_state)
  , module_   :: VarChar 33
  , action    :: VarChar 25
  , role      :: VarChar 25
  , state     :: Maybe (VarChar 25)
  , access    :: Bool
  , stamp     :: GrecGroup Stamp       --
  , status    :: Bool                  -- Вкл./выкл.

  } deriving (Show, Eq, Generic)

type TAccessTab = TableD W_access '["id"] '[] True
type TAccess = DataD TAccessTab
    ( ModuleRef DcCascade ': ActionRef DcCascade ': RoleRef DcCascade
    ': StateRef DcCascade ':  StampFK
    )

type AccessRef (x :: DelCons) = '(TAccessTab, '[ '("id_access","id")], x)
