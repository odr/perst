{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
-- {-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module OrderTree where

import           Data.Int               (Int64)
import qualified Data.Text              as T
import           GHC.Generics           (Generic)

import           Data.Type.Grec         (Grec)
import           Perst.Database.DML     (DML (..))
import           Perst.Database.Sqlite  (Sqlite)
import           Perst.Database.TreeDef (TreeDef' (..))

import           Order

data OrderTree = OrderTree
  { id        :: Int64
  , num       :: T.Text
  , date      :: T.Text
  , positions :: [Grec OrderPosition]
  } deriving (Show, Generic, Eq, Ord)

type TOrderTree = TreeDefC TOrder
  '[ '("positions", '(TreeDefC TOrderPosition '[],  '[ '("orderId", "id") ]))]

instance DML Sqlite TOrder (Grec OrderTree)
