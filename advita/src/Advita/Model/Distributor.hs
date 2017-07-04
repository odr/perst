module Advita.Model.Distributor where

import           Data.Text              (Text)
import           GHC.Generics           (Generic)

import           Data.Type.Grec         (GrecGroup)
import           Perst.Database.DataDef (DataD, DelCons (..), TableD)

import           Advita.Model.Fields    (Date, Ident, VarChar)
import           Advita.Model.User      (Stamp, StampFK)

data D_distributor = D_distributor
  { id      :: Ident D_distributor
  , name    :: VarChar 255
  , address :: Maybe (VarChar 200) -- Диагноз
  , stamp   :: GrecGroup Stamp     --
  , status  :: Bool                -- Вкл./выкл.
  } deriving (Show, Eq, Generic)

type TDistributorTab = TableD D_distributor '["id"] '[] True

type TDistributor = DataD TDistributorTab StampFK

type DistributorRef (x :: DelCons) = '(TDistributorTab, '[ '("distributor_id", "id")], x)
