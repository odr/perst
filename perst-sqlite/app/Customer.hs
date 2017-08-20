{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Customer where

import           Data.Int                (Int64)
-- import           Data.Singletons.Prelude
-- import           Data.Singletons.TypeRepStar ()
import           Data.Tagged             (Tagged)
import qualified Data.Text               as T
import           GHC.Generics            (Generic)
import           GHC.TypeLits            (Symbol)

import           Data.Type.Grec          (ConvFromGrec, ConvGrecInfo, Grec,
                                          GrecGroup (..))
import           Perst.Database.DataDef  (DataDef' (..), DataInfo (..),
                                          DelCons (..), FK (..))
import           Perst.Database.DbOption (DbOption (..))
import           Perst.Database.DDL      (DDL (..))
import           Perst.Database.DML      (DML (..))
import           Perst.Database.Sqlite   (Sqlite)

data Customer = Customer
  { id    :: Int64
  , names :: GrecGroup Names
  , note  :: Maybe T.Text
  , note2 :: Maybe T.Text
  , note3 :: Maybe T.Text
  -- , email :: T.Text
  } deriving (Show, Generic)

data Names = Names
  { name      :: T.Text
  , shortname :: Maybe T.Text
  } deriving (Show, Generic)


-- pCustomer      = sing :: Sing TCustomer

data Address = Address
  { id         :: Int64
  , customerId :: Int64
  , street     :: T.Text
  , house      :: T.Text
  } deriving (Show, Generic, Eq, Ord)

-- instance SymLens "id" Address Int64 where
--   symLens _ f a = (\b -> (a :: Address) { Customer.id = b })
--               <$> f (Customer.id (a :: Address))
--
-- pAddress       = sing :: Sing TAddress

-- type TCustomerTab = TableD Customer '["id"] '[ '["name"]] False
-- type TCustomer = DataD TCustomerTab '[]
--
-- type TAddressTab = TableD Address '["id"] '[] False
-- type TAddress = DataD TAddressTab
--                       '[ '(TCustomerTab, '[ '("customerId", "id")], DcCascade)]

type TCustomer = '(Customer, DataDefC (TableInfo '["id"] '[ '["name"]] False) '[])

type TAddress =
  '(Address
  , DataDefC (TableInfo '["id"] '[] False)
            '[ FKC "customer" DcCascade '[ '("customerId", "id")]]
  )

instance DML Sqlite TCustomer Customer
instance DML Sqlite TAddress Address
instance DDL Sqlite TCustomer
instance DDL Sqlite TAddress
