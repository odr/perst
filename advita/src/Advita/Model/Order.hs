module Advita.Model.Order where

import           GHC.Generics             (Generic)

import           Data.Type.Grec           (GrecGroup)
import           Perst.Database.DataDef   (DataD, DelCons (..), TableD)

import           Advita.Model.Distributor (D_distributor, DistributorRef)
import           Advita.Model.Fields      (Ident, SizedInt, VarChar)
import           Advita.Model.State       (StateRef, W_state)
import           Advita.Model.User        (Stamp, StampFK)

data D_order = D_oOrder
  { id             :: Ident D_order
  , id_distributor :: Ident D_distributor
  , stamp          :: GrecGroup Stamp       --
  , status         :: Bool                  -- Вкл./выкл.
  , id_state       :: Maybe (Ident W_state)
  , pseudo_id      :: SizedInt 10
  , pseudo_cid     :: VarChar 20

  } deriving (Show, Eq, Generic)

type TOrderTab = TableD D_order '["id"] '[] True

type TOrder = DataD TOrderTab
                  (DistributorRef DcRestrict ': StateRef DcRestrict ': StampFK)

type OrderRef (x::DelCons) = '(TOrderTab, '[ '("id_order", "id")], x)
