module Advita.Model.User where

import           Data.Text               (Text, pack, unpack)
import           GHC.Generics            (Generic)
import           GHC.TypeLits            (Symbol)

import           Data.Type.Grec          (GrecGroup)
import           Perst.Database.DataDef  (DataD, DelCons (..), TableD)
import           Perst.Database.DbOption (DBEnum)
import           Perst.Database.Sqlite   (Sqlite)

import           Advita.Model.Fields

data U_user = U_user
  { u_id     :: Ident U_user
  , u_atime  :: Maybe DateTime         -- Последний вход
  , u_status :: Bool                   -- Вкл./выкл.
  , u_name   :: VarChar 50             -- Отображаемое имя
  , u_fname  :: VarChar 100            -- Имя
  , u_sname  :: Maybe (VarChar 100)    -- Отчество
  , u_lname  :: Maybe (VarChar 100)    -- Фамилия
  , u_bdate  :: Maybe Date             -- Дата рождения
  , u_gender :: Maybe Gender           -- Пол
  -- , u_prop        :: Text             --
  , stamp    :: GrecGroup Stamp
  } deriving (Show, Eq, Generic)

data Stamp = Stamp
  { ctime      :: Timestamp              -- Время создания
  , mtime      :: DateTime               -- Время обновления
  , id_creator :: Ident U_user           -- Создатель записи
  , id_editor  :: Ident U_user           -- Редактор записи
  } deriving (Show, Eq, Generic)

type Gender       = DBEnum ["m","f"]      -- male, female

type TUserTab = TableD U_user '["u_id1"] '[] True

type UserRef (x::DelCons) = '(TUserTab, '[ '("id_user", "u_id")], x)

type StampFK
  = '[ '( TUserTab, '[ '("id_creator", "u_id")], DcRestrict)
     , '( TUserTab, '[ '("id_editor" , "u_id")], DcRestrict)
     ]

type TUser = DataD TUserTab StampFK
