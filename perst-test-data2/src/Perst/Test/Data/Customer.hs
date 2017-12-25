{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module Perst.Test.Data.Customer where

import           Data.Aeson             (FromJSON, ToJSON)
import           Data.Int               (Int64)
import           Data.Tagged            (Tagged)
import qualified Data.Text              as T
import           GHC.Generics           (Generic)
import           GHC.TypeLits           (Symbol)

import           Data.Type.GrecTree     (ConvNames (..), GPlus, Grec (..))
import           Perst.Database.DataDef (DataInfo (..), DataStruct' (..),
                                         DelCons (..))
import           Perst.Database.DDL     (DDL (..))

import           Perst.Test.Data.Db     (Db)

data Customer = Customer
  { id    :: Int64
  , names :: Tagged (Nothing::Maybe Symbol) Names
  , note  :: Maybe T.Text
  , note2 :: Maybe T.Text
  , note3 :: Maybe T.Text
  , email :: Maybe T.Text
  } deriving (Show, Generic)

data Names = Names
  { name      :: T.Text
  , shortname :: Maybe T.Text
  } deriving (Show, Generic, Eq)
instance ToJSON Names
instance FromJSON Names
type instance GPlus Names = True

data Address = Address
  { id         :: Int64
  , customerId :: Int64
  , street     :: T.Text
  , house      :: T.Text
  } deriving (Show, Generic, Eq, Ord)
instance ToJSON Address
instance FromJSON Address

type TCustomer = DataStructC (TableInfo "customer" '["id"] '[ '["name"]] True)
                        Customer

type TAddress = DataStructC (TableInfo "address" '["id"] '[] False) Address

                -- '[ FKC "customer" DcCascade '[ '("customerId", "id")]]


instance Grec Customer
instance Grec Names
instance ConvNames t Customer

instance Grec Address
instance ConvNames t Address
-- instance DDL Db TCustomer Customer
-- instance DDL Db TAddress Address

instance ToJSON Customer
instance FromJSON Customer

-- instance DML Db TCustomer Customer
-- instance DMLTree Db (TreeDefC TCustomer '[]) Customer
