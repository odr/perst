{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
-- {-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module CustomerTree where

import           Data.Int                (Int64)
-- import           Data.Tagged             (Tagged)
import qualified Data.Text               as T
import           GHC.Generics            (Generic)
-- import           GHC.TypeLits            (Symbol)

import           Data.Type.Grec          (Grec)
import           Perst.Database.DML      (DML (..))
import           Perst.Database.DMLTree  (DMLTree (..))
import           Perst.Database.Sqlite   (Sqlite)
import           Perst.Database.Tree.Def (TreeDef' (..))

import           Customer
import           OrderTree

data CustomerTree = CustomerTree
  { id        :: Int64
  , name      :: T.Text
  , shortname :: Maybe T.Text
  , orders    :: [Grec OrderTree]
  , address   :: [Grec Address]
  } deriving (Show, Generic, Eq, Ord)

type TCustomerTree = TreeDefC TCustomer
  '[ '( "orders",  '(TOrderTree, '[ '("customerId","id") ]))
   , '( "address", '(TreeDefC TAddress '[], '[ '("customerId","id") ]))
   ]
--

instance DML Sqlite TCustomer (Grec CustomerTree)

instance DMLTree Sqlite TCustomerTree (Grec CustomerTree)
