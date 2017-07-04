module Advita.Model.Drug where

import           Data.Text              (Text)
import           GHC.Generics           (Generic)

import           Data.Type.Grec         (GrecGroup)
import           Perst.Database.DataDef (DataD, DelCons (..), TableD)

import           Advita.Model.Fields    (Date, Ident, SizedInt, VarChar)
import           Advita.Model.Order     (D_order, OrderRef)
import           Advita.Model.Request   (D_request, RequestRef)
import           Advita.Model.State     (StateRef, W_state)
import           Advita.Model.User      (Stamp, StampFK, U_user)

data Drug = Drug
  { id          :: Ident Drug
  , id_order    :: Maybe (Ident D_order)
  , id_request  :: Ident D_request
  , id_drug     :: SizedInt 10
  , count_drug  :: SizedInt 5             -- Кол-во препаратов
  , stamp       :: GrecGroup Stamp        --
  , status      :: Bool                   -- Вкл./выкл.
  , id_state    :: Ident W_state          -- Состояние записи
  , reason      :: VarChar 500            -- Основание для заказа лекарства
  , duration    :: VarChar 250            -- Длительность применения
  -- , price  :: SizedDouble 10 2
  , datetransit :: Date
  } deriving (Show, Eq, Generic)

type TDrugTab = TableD Drug '["id"] '[] True

type TDrug = DataD TDrugTab
    ( OrderRef DcRestrict ': StateRef DcRestrict ': RequestRef DcRestrict
    ': StampFK
    )
