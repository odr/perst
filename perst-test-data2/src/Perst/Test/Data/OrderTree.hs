{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
-- {-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Perst.Test.Data.OrderTree where

import           Data.Aeson            (FromJSON, ToJSON)
import           Data.Int              (Int64)
import qualified Data.Text             as T
import           GHC.Generics          (Generic)

import           Data.Type.GrecTree    (ConvNames (..), Grec (..))
-- import           Perst.Database.TreeDef (TreeDef' (..))
import           Perst.Types           (PChilds)

import           Perst.Test.Data.Order (OrderPosition, TOrder, TOrderPosition)

data OrderTree = OrderTree
  { id    :: Int64
  , num   :: T.Text
  , date  :: T.Text
  , opOrd :: PChilds OrderPosition
  } deriving (Show, Generic, Eq, Ord)

-- type TOrderTree = TreeDefC TOrder
--   '[ '("positions", '(TreeDefC TOrderPosition '[],  '[ '("orderId", "id") ]))]

instance ToJSON OrderTree
instance FromJSON OrderTree

instance Grec OrderTree
instance ConvNames t OrderTree
