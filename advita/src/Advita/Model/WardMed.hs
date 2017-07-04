module Advita.Model.WardMed where

import           Data.Text              (Text)
import           GHC.Generics           (Generic)

import           Data.Type.Grec         (GrecGroup)
import           Perst.Database.DataDef (DataD, DelCons (..), TableD)

import           Advita.Model.Fields    (Date, Ident, VarChar)
import           Advita.Model.Hospital  (D_hospital, HospitalRef)
import           Advita.Model.User      (Stamp, StampFK, TUserTab, U_user)
import           Advita.Model.Ward      (Ward, WardRef)

data Ward_medinfo = Ward_medinfo
  { id           :: Ident Ward_medinfo
  , id_ward      :: Ident Ward
  , diacrisis    :: VarChar 200         -- Диагноз
  , id_hospital  :: Ident D_hospital
  , id_attending :: Ident U_user        -- Лечащий врач
  , blood        :: VarChar 10          -- Группа крови
  , charge       :: VarChar 100         -- Вид лечения
  , state        :: VarChar 100         -- Состояние
  , since        :: Maybe Date          -- Действительно с
  , till         :: Maybe Date          -- Действительно до
  , stamp        :: GrecGroup Stamp     --
  , status       :: Bool                -- Вкл./выкл.
  } deriving (Show, Eq, Generic)

type TWard_medinfo = DataD (TableD Ward_medinfo '["id"] '[] True)
        ( WardRef DcCascade ': HospitalRef DcRestrict
        ': '(TUserTab, '[ '("id_attending", "u_id")], DcRestrict)
        ':  StampFK
        )
