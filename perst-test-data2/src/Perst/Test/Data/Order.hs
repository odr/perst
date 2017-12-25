{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}
module Perst.Test.Data.Order where

import           Data.Aeson             (FromJSON, ToJSON)
import           Data.Int               (Int64)
import qualified Data.Text              as T
import           GHC.Generics           (Generic)

import           Data.Type.GrecTree     (ConvNames (..), Grec (..))
import           Perst.Database.DataDef (DataInfo (..), DataStruct' (..),
                                         DelCons (..))
import           Perst.Database.DDL     (DDL (..))

import           Perst.Test.Data.Db     (Db)

data Orders = Order -- name ORDER is disabled in sqlite!
  { id           :: Int64
  , num          :: T.Text
  , customerId   :: Int64
  , coCustomerId :: Maybe Int64
  , date         :: T.Text
  } deriving (Show, Generic)

data OrderPosition = OrderPosition
  { orderId   :: Int64
  , articleId :: Int64
  , quantity  :: Int64
  , cost      :: Double
  } deriving (Show, Generic, Eq, Ord)

instance ToJSON OrderPosition
instance FromJSON OrderPosition

type TOrder = DataStructC
                  (TableInfo "orders" '["id"] '[ '["customerId", "num"]] True)
                  Orders

             -- '[ FKC "customer" DcCascade '[ '("customerId", "id")]
             -- ,  FKC "customer" DcSetNull '[ '("coCustomerId", "id")]
             -- ]

type TOrderPosition = DataStructC
        (TableInfo "OrderPosition" '["orderId","articleId"] '[] False)
        OrderPosition
            -- '[ FKC "orders" DcCascade '[ '("orderId"  ,"id")]
            --  , FKC "article" DcRestrict '[ '("articleId","id")]
            --  ]

instance Grec OrderPosition
instance Grec Orders
instance ConvNames t OrderPosition
instance ConvNames t Orders
-- instance DDL Db TOrderPosition
-- instance DDL Db TOrder
