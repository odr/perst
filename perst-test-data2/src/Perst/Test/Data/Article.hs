{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}
module Perst.Test.Data.Article where

import           Data.Int               (Int64)
import qualified Data.Text              as T
import           GHC.Generics           (Generic)

import           Data.Type.GrecTree     (ConvNames (..), Grec (..))
import           Perst.Database.DataDef (DataDef' (..), DataInfo (..))
import           Perst.Database.DDL     (DDL (..))

import           Perst.Test.Data.Db     (Db)


data Article = Article
  { id       :: Int64
  , name     :: T.Text
  , price    :: Double
  , producer :: T.Text
  , note     :: Maybe T.Text
  } deriving (Show, Generic)

type TArticle = DataDefC (TableInfo "article" '["id"] '[ '["name"]] False) '[]

instance Grec Article
instance ConvNames t Article
instance DDL Db TArticle Article
-- instance DML Db TArticle (GrecTagged Article)
