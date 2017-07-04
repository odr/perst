module Advita.Model.WardHistory where

import           GHC.Generics            (Generic)

import           Data.Type.Grec          (GrecGroup)
import           Perst.Database.DataDef  (DataD, DelCons (..), TableD)
import           Perst.Database.DbOption (DBEnum)

import           Advita.Model.Fields     (Date, Ident, MediumText, VarChar)
import           Advita.Model.State      (StateRef, W_state)
import           Advita.Model.User       (Stamp, StampFK)
import           Advita.Model.Ward       (Ward, WardRef)

data WardHistory = WardHistory
  { id       :: Ident WardHistory
  , id_ward  :: Ident Ward
  , state    :: DBEnum '["0","1","2"] -- 0 - не актуальные записи, 1 - актуальные, но просроченные записи, 2 - актуальные
  , note     :: Maybe (VarChar 500)
  , internal :: Maybe MediumText      -- Внутренняя информация
  , external :: Maybe MediumText      -- Внешняя информация
  , source   :: Maybe MediumText      -- Источник информации
  , since    :: Maybe Date            -- Дата события
  , till     :: Maybe Date            -- Дата следующего обновления
  , stamp    :: GrecGroup Stamp       --
  , status   :: Bool                  -- Вкл./выкл.
  , id_state :: Maybe (Ident W_state) -- Состояние записи
  } deriving (Show, Eq, Generic)

type TWardHistory = DataD (TableD WardHistory '["id"] '[] True)
                          (WardRef DcCascade ': StateRef DcRestrict ':  StampFK)
