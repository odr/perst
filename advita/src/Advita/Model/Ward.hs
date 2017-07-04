module Advita.Model.Ward where

import           Data.Text              (Text)
import           GHC.Generics           (Generic)

import           Data.Type.Grec         (GrecGroup)
import           Perst.Database.DataDef (DataD, DelCons (..), TableD)

import           Advita.Model.Fields    (Date, Ident, VarChar)
import           Advita.Model.User      (Stamp, StampFK)

data Ward = Ward
  { id               :: Ident Ward
  , name             :: VarChar 255         -- Имя подопечного
  , birthday         :: Date
  , address          :: Maybe (VarChar 255) -- Адрес проживания
  , identity         :: VarChar 255         -- Удостоверение личности
  , identity_address :: Maybe (VarChar 255) -- Адрес регистрации
  , fund             :: Maybe (VarChar 100) --
  , stamp            :: GrecGroup Stamp     --
  , status           :: Bool                -- Вкл./выкл.
  , rel_liability    :: Bool                -- Родственник отвечает за подопечного
  , web_page_eng     :: Maybe Bool          -- Есть английская страница
  , donation         :: Maybe Bool          -- Требуется сбор средств
  , web_page         :: Maybe Bool
  } deriving (Show, Eq, Generic)

type TWardTab = TableD Ward '["id"] '[] True

type TWard = DataD TWardTab StampFK

type WardRef (x::DelCons) = '(TWardTab, '[ '("id_ward", "id")], x)

data Ward_need = Ward_need
  { id      :: Ident Ward_need
  , id_ward :: Ident Ward
  , drugs   :: Text                 -- Лекарства
  , donor   :: Text                 -- Донор
  , medhelp :: Text                 -- Мед. помощь
  , finhelp :: Text                 -- Фин. помощь
  , survey  :: Date                 -- Обследования
  , stamp   :: GrecGroup Stamp      --
  , status  :: Bool                 -- Вкл./выкл.
  } deriving (Show, Eq, Generic)

type TWardNeed = DataD (TableD Ward_need '["id"] '[] True)
                       (WardRef DcCascade ': StampFK)

data Ward_relative = Ward_relative
  { id               :: Ident Ward_relative
  , id_ward          :: Ident Ward
  , name             :: VarChar 255
  , degree           :: VarChar 255
  , caretaker        :: Bool
  , since            :: Date
  , till             :: Date
  , address          :: Maybe (VarChar 255) -- Адрес проживания
  , identity         :: Maybe (VarChar 255) -- Удостоверение личности
  , identity_address :: Maybe (VarChar 255) -- Адрес регистрации
  , stamp            :: GrecGroup Stamp     --
  , status           :: Bool                -- Вкл./выкл.
  } deriving (Show, Eq, Generic)

type TWardRelative = DataD (TableD Ward_relative '["id"] '[] True)
                           (WardRef DcCascade ': StampFK)
