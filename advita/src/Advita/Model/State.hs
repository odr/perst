module Advita.Model.State where

import           Data.Text              (Text)
import           GHC.Generics           (Generic)

import           Perst.Database.DataDef (DataD, DelCons (..), TableD)

import           Advita.Model.Fields    (Ident, VarChar)
import           Advita.Model.Module    (ModuleRef, W_module)

data W_state = W_state
  { id          :: Ident W_state
  , id_module   :: Ident W_module
  , name        :: VarChar 50           -- Название состояния
  , description :: Maybe (VarChar 255)  -- Описание состояния
  , initial     :: Bool                 -- Исходное состояние?
  , color       :: Maybe (VarChar 6)    -- Цвет состояния
  } deriving (Show, Eq, Generic)

type TStateTab = TableD W_state '["id"] '[ '["id_module","name"]] True
type TState = DataD TStateTab '[ModuleRef DcCascade]

type StateRef (x::DelCons) = '(TStateTab, '[ '("id_state", "id")], x)
