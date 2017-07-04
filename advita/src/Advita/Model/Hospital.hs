module Advita.Model.Hospital where

import           GHC.Generics           (Generic)

import           Data.Type.Grec         (GrecGroup)
import           Perst.Database.DataDef (DataD, DelCons (..), TableD)

import           Advita.Model.Fields    (Date, Ident, VarChar)
import           Advita.Model.User      (Stamp, StampFK, U_user, UserRef)

data D_hospital = D_hospital
  { id     :: Ident D_hospital
  , name   :: VarChar 255         -- Имя подопечного
  , stamp  :: GrecGroup Stamp     --
  , status :: Bool                -- Вкл./выкл.
  , parent :: Maybe (Ident D_hospital)
  , mpath  :: Maybe (VarChar 255)
  , order  :: Maybe (VarChar 255)
  } deriving (Show, Eq, Generic)

type THospitalTab = TableD D_hospital '["id"] '[] True

type THospital = DataD THospitalTab
                ( '(THospitalTab, '[ '("parent", "id")], DcCascade) ': StampFK)

type HospitalRef (x::DelCons) = '(THospitalTab, '[ '("id_hospital", "id")], x)

data D_medic = D_medic
  { id          :: Ident D_medic
  , id_user     :: Ident U_user
  , id_hospital :: Ident D_hospital
  , till        :: Maybe Date
  , since       :: Maybe Date
  , stamp       :: GrecGroup Stamp     --
  , status      :: Bool                -- Вкл./выкл.
  } deriving (Show, Eq, Generic)

type TMedicTab = TableD D_medic '["id"] '[] True

type TMedic = DataD TMedicTab
                    (HospitalRef DcCascade ': UserRef DcRestrict ': StampFK)
