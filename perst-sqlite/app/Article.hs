{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Article where

import           Data.Int                (Int64)
-- import           Data.Singletons.Prelude
-- import           Data.Singletons.TypeRepStar ()
import qualified Data.Text               as T
import           GHC.Generics            (Generic)

import           Data.Type.Grec          (ConvFromGrec, ConvGrecInfo)
import           Perst.Database.DataDef  (DataDef' (..), DataInfo (..),
                                          DelCons (..))
import           Perst.Database.DbOption (DbOption (..))
import           Perst.Database.DML      (DeleteByKey (..), Insert (..),
                                          UpdateByKeyDiff (..))
import           Perst.Database.Sqlite   (Sqlite)


data Article = Article
  { id       :: Int64
  , name     :: T.Text
  , price    :: Double
  , producer :: T.Text
  , note     :: Maybe T.Text
  } deriving (Show, Generic)

-- pArticle       = sing :: Sing TArticle
--
-- type TArticleTab = TableD Article '["id"] '[ '["name"]] False
-- type TArticle = DataD TArticleTab '[]

type TArticle = '(Article, DataDefC (TableInfo '["id"] '[ '["name"]] False) '[])

instance (ConvGrecInfo r, ConvFromGrec r [FieldDB Sqlite])
        => Insert Sqlite TArticle r

instance (ConvGrecInfo r1, ConvGrecInfo r2
        , ConvFromGrec r1 [FieldDB Sqlite], ConvFromGrec r2 [FieldDB Sqlite])
        => UpdateByKeyDiff Sqlite TArticle r1 r2

instance (ConvGrecInfo r, ConvFromGrec r [FieldDB Sqlite])
        => DeleteByKey Sqlite TArticle r
