module Advita.Model.Request where

import           Data.Text              (Text)
import           GHC.Generics           (Generic)

import           Data.Type.Grec         (GrecGroup)
import           Perst.Database.DataDef (DataD, DelCons (..), TableD)

import           Advita.Model.Fields    (Date, Ident, SizedInt, VarChar)
import           Advita.Model.Hospital  (D_hospital, HospitalRef)
import           Advita.Model.State     (StateRef, W_state)
import           Advita.Model.User      (Stamp, StampFK, TUserTab, U_user)
import           Advita.Model.Ward      (Ward, WardRef)

data D_request = D_request
  { id          :: Ident D_request
  -- , ward        :: VarChar 255            -- Пациент
  , id_ward     :: Ident Ward               -- ID подопечного
  , id_hospital :: Maybe (Ident D_hospital) -- Клиника
  , id_owner    :: Ident U_user             -- Врач (Пользователь)
  , stamp       :: Stamp          --
  , status      :: Bool                     -- Вкл./выкл.
  , id_state    :: Ident W_state            -- Состояние записи
  , datetowork  :: Maybe Date

  } deriving (Show, Eq, Generic)

type TRequestTab = TableD D_request '["id"] '[] True
type TRequest = DataD TRequestTab
    ( WardRef DcRestrict ': StateRef DcRestrict ': HospitalRef DcRestrict
    ': '(TUserTab, '[ '("id_owner", "id")], DcRestrict)
    ':  StampFK
    )

type RequestRef (x::DelCons) = '(TRequestTab, '[ '("id_request", "id")], x)
