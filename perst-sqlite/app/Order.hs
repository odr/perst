{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Order where

import           Data.Int                (Int64)
-- import           Data.Singletons.Prelude
import           Data.Tagged             (Tagged)
import qualified Data.Text               as T
import           GHC.Generics            (Generic)
import           GHC.TypeLits            (Symbol)

import           Data.Type.Grec          (ConvFromGrec, ConvGrecInfo, Grec,
                                          GrecGroup (..))
import           Perst.Database.DataDef  (DataDef' (..), DataInfo (..),
                                          DelCons (..), FK (..))
import           Perst.Database.DbOption (DbOption (..))
import           Perst.Database.DML      (DeleteByKey (..), Insert (..),
                                          SelectByKey (..),
                                          UpdateByKeyDiff (..))
import           Perst.Database.Sqlite   (Sqlite)

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
  '(Orders
  , DataDefC (TableInfo '["id"] '[ '["customerId", "num"]] True)
             '[ FKC "customer" DcCascade '[ '("customerId", "id")]
             ,  FKC "customer" DcSetNull '[ '("coCustomerId", "id")]
             ]
   )

type TOrderPosition =
  '(OrderPosition
  , DataDefC (TableInfo '["orderId","articleId"] '[] False)
            '[ FKC "orders" DcCascade '[ '("orderId"  ,"id")]
             , FKC "article" DcRestrict '[ '("articleId","id")]
             ]
  )

instance (ConvGrecInfo r, ConvFromGrec r [FieldDB Sqlite])
        => Insert Sqlite TOrder r
instance (ConvGrecInfo r, ConvFromGrec r [FieldDB Sqlite])
        => Insert Sqlite TOrderPosition r

instance (ConvGrecInfo r1, ConvGrecInfo r2
        , ConvFromGrec r1 [FieldDB Sqlite], ConvFromGrec r2 [FieldDB Sqlite])
        => UpdateByKeyDiff Sqlite TOrder r1 r2
instance (ConvGrecInfo r1, ConvGrecInfo r2
        , ConvFromGrec r1 [FieldDB Sqlite], ConvFromGrec r2 [FieldDB Sqlite])
        => UpdateByKeyDiff Sqlite TOrderPosition r1 r2

instance (ConvGrecInfo r, ConvFromGrec r [FieldDB Sqlite])
        => DeleteByKey Sqlite TOrder r
instance (ConvGrecInfo r, ConvFromGrec r [FieldDB Sqlite])
        => DeleteByKey Sqlite TOrderPosition r
