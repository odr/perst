{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}
module Perst.Test.Data.Article where

import           Data.Int                (Int64)
-- import           Data.Singletons.Prelude
-- import           Data.Singletons.TypeRepStar ()
import qualified Data.Text               as T
import           GHC.Generics            (Generic)

import           Data.Type.Grec          ((:::), ConvFromGrec, ConvGrecInfo,
                                          Grec)
import           Perst.Database.DataDef  (DataDef' (..), DataInfo (..),
                                          DelCons (..))
import           Perst.Database.DbOption (DbOption (..))
import           Perst.Database.DDL      (DDL (..))
import           Perst.Database.DML      (DML)

import           Perst.Test.Data.Db      (Db)


data Article = Article
  { id       :: Int64
  , name     :: T.Text
  , price    :: Double
  , producer :: T.Text
  , note     :: Maybe T.Text
  } deriving (Show, Generic)

-- type TArticle = '(Article, DataDefC (TableInfo '["id"] '[ '["name"]] False) '[])
type TArticle
  = DataDefC (TableInfo "article" '["id"] '[ '["name"]] False) '[]

instance DML Db TArticle Article
instance DDL Db TArticle Article
