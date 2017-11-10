{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
-- {-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Perst.Test.Data.OrderTree where

import           Data.Aeson             (FromJSON, ToJSON)
import           Data.Int               (Int64)
import qualified Data.Text              as T
import           GHC.Generics           (Generic)

-- import           Data.Type.Grec         (Grec)
import           Perst.Database.DML     (DML (..))
import           Perst.Database.TreeDef (TreeDef' (..))

import           Perst.Test.Data.Db     (Db)
import           Perst.Test.Data.Order

data OrderTree = OrderTree
  { id        :: Int64
  , num       :: T.Text
  , date      :: T.Text
  , positions :: [OrderPosition]
  } deriving (Show, Generic, Eq, Ord)

type TOrderTree = TreeDefC TOrder
  '[ '("positions", '(TreeDefC TOrderPosition '[],  '[ '("orderId", "id") ]))]

instance DML Db TOrder OrderTree

instance ToJSON OrderTree
instance FromJSON OrderTree
