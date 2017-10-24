{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}
module Perst.Test.Data.Order where

import           Data.Int                (Int64)
-- import           Data.Singletons.Prelude
import           Data.Tagged             (Tagged)
import qualified Data.Text               as T
import           GHC.Generics            (Generic)
import           GHC.TypeLits            (Symbol)

import           Data.Type.Grec          ((:::), ConvFromGrec, ConvGrecInfo,
                                          Grec)
import           Perst.Database.DataDef  (DataDef' (..), DataInfo (..),
                                          DelCons (..), FK (..))
import           Perst.Database.DbOption (DbOption (..))
import           Perst.Database.DDL      (DDL (..))
import           Perst.Database.DML      (DML (..))

import           Perst.Test.Data.Db      (Db)

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

type TOrder =
  DataDefC (TableInfo "orders" '["id"] '[ '["customerId", "num"]] True)
             '[ FKC "customer" DcCascade '[ '("customerId", "id")]
             ,  FKC "customer" DcSetNull '[ '("coCustomerId", "id")]
             ]

type TOrderPosition =
  DataDefC (TableInfo "OrderPosition" '["orderId","articleId"] '[] False)
            '[ FKC "orders" DcCascade '[ '("orderId"  ,"id")]
             , FKC "article" DcRestrict '[ '("articleId","id")]
             ]

instance DML Db TOrderPosition OrderPosition
instance DDL Db TOrderPosition OrderPosition
instance DDL Db TOrder Orders
