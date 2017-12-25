{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Perst.Test.Data.CustomerTree where

import           Data.Aeson                (FromJSON, ToJSON)
import           Data.Int                  (Int64)
import           Data.Tagged               (Tagged)
import           GHC.Generics              (Generic)
import           GHC.TypeLits              (Symbol)

import           Data.Type.GrecTree        (ConvNames (..), Grec (..))
-- import           Perst.Database.TreeDef    (TreeDef' (..))
import           Perst.Types               (PChilds)

import           Perst.Test.Data.Customer
import           Perst.Test.Data.OrderTree

data CustomerTree = CustomerTree
  { id      :: Int64
  , names   :: Tagged (Nothing::Maybe Symbol) Names
  , ordCust :: PChilds OrderTree
  , addCust :: PChilds Address
  } deriving (Show, Generic, Eq)

-- type TCustomerTree = TreeDefC TCustomer
--   '[ '( "orders",  '(TOrderTree, '[ '("customerId","id") ]))
--    , '( "address", '(TreeDefC TAddress '[], '[ '("customerId","id") ]))
--    ]

instance ToJSON CustomerTree
instance FromJSON CustomerTree
instance Grec CustomerTree
instance ConvNames t CustomerTree
