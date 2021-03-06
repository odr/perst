{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}
module Article where

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
import           Perst.Database.Sqlite   (Sqlite)


data Article = Article
  { id       :: Int64
  , name     :: T.Text
  , price    :: Double
  , producer :: T.Text
  , note     :: Maybe T.Text
  } deriving (Show, Generic)

-- type TArticle = '(Article, DataDefC (TableInfo '["id"] '[ '["name"]] False) '[])
type TArticle
  = '( '[ "id"    ::: Int64
        , "name"  ::: T.Text
        , "price" ::: Double
        , "producer" ::: T.Text
        , "note"     ::: Maybe T.Text
        ]
    , DataDefC (TableInfo "article" '["id"] '[ '["name"]] False) '[]
    )

instance DML Sqlite TArticle (Grec Article)
instance DDL Sqlite TArticle
