{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}
module Perst.Test.Data.Customer where

import           Data.Int                (Int64)
-- import           Data.Singletons.Prelude
-- import           Data.Singletons.TypeRepStar ()
import           Data.Aeson              (FromJSON, ToJSON)
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

data Customer = Customer
  { id    :: Int64
  , names :: Grec Names
  , note  :: Maybe T.Text
  , note2 :: Maybe T.Text
  , note3 :: Maybe T.Text
  -- , email :: T.Text
  } deriving (Show, Generic)

data Names = Names
  { name      :: T.Text
  , shortname :: Maybe T.Text
  } deriving (Show, Generic, Eq)
instance ToJSON Names
instance FromJSON Names

data Address = Address
  { id         :: Int64
  , customerId :: Int64
  , street     :: T.Text
  , house      :: T.Text
  } deriving (Show, Generic, Eq, Ord)
instance ToJSON Address
instance FromJSON Address

type TCustomer = DataDefC (TableInfo "customer" '["id"] '[ '["name"]] False) '[]

type TAddress = DataDefC (TableInfo "address" '["id"] '[] False)
                '[ FKC "customer" DcCascade '[ '("customerId", "id")]]

-- type TCustomer = '(Customer, DataDefC (TableInfo '["id"] '[ '["name"]] False) '[])
--
-- type TAddress =
--   '(Address
--   , DataDefC (TableInfo '["id"] '[] False)
--             '[ FKC "customer" DcCascade '[ '("customerId", "id")]]
--   )

-- instance DML Sqlite TCustomer (Grec Customer)
instance DML Db TAddress Address
instance DDL Db TCustomer Customer
instance DDL Db TAddress Address

instance ToJSON Customer
instance FromJSON Customer
