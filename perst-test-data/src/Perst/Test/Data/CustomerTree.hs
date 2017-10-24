{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
-- {-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Perst.Test.Data.CustomerTree where

import           Data.Int                  (Int64)
import qualified Data.Text                 as T
import           GHC.Generics              (Generic)

import           Data.Type.Grec            (Grec)
import           Perst.Database.DML        (DML (..))
import           Perst.Database.DMLTree    (DMLTree (..))
import           Perst.Database.TreeDef    (TreeDef' (..))

import           Perst.Test.Data.Customer
import           Perst.Test.Data.Db        (Db)
import           Perst.Test.Data.OrderTree

data CustomerTree = CustomerTree
  { id      :: Int64
  , names   :: Grec Names
  , orders  :: [OrderTree]
  , address :: [Address]
  } deriving (Show, Generic, Eq)

type TCustomerTree = TreeDefC TCustomer
  '[ '( "orders",  '(TOrderTree, '[ '("customerId","id") ]))
   , '( "address", '(TreeDefC TAddress '[], '[ '("customerId","id") ]))
   ]
--

instance DML Db TCustomer CustomerTree

instance DMLTree Db TCustomerTree CustomerTree
