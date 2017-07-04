module Advita.Model.Role where

import           GHC.Generics           (Generic)

import           Data.Type.Grec         (GrecGroup)
import           Perst.Database.DataDef (DataD, DelCons (..), TableD)

import           Advita.Model.Fields    (Ident, Timestamp, VarChar)
import           Advita.Model.User      (Stamp, StampFK, U_user, UserRef)

data U_role = U_role
  { id          :: Ident U_role
  , code        :: VarChar 50
  , name        :: VarChar 50
  , description :: Maybe (VarChar 50)
  , synthetic   :: Bool
  , stamp       :: GrecGroup Stamp     --
  , status      :: Bool                -- Вкл./выкл.
  } deriving (Show, Eq, Generic)

type TRoleTab = TableD U_role '["id"] '[ '["code"], '["name"]] True

type TRole = DataD TRoleTab StampFK

type RoleRef (x::DelCons) = '(TRoleTab, '[ '("id_role", "id")], x)

data U_user_role = U_user_role
  { id      :: Ident U_user_role
  , ctime   :: Timestamp
  , id_user :: Ident U_user
  , id_role :: Ident U_role
  }

type TUserRole = DataD (TableD U_user_role '["id"] '[] True)
                      '[ UserRef DcCascade, RoleRef DcCascade]
