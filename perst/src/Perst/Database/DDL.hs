{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}
module Perst.Database.DDL
    ( DDL (..)
    ) where

import           Control.Arrow              (first)
import           Control.Monad.IO.Class     (MonadIO)
import           Data.Kind                  (Type)
import           Data.List                  (intercalate)
import           Data.Proxy                 (Proxy (..))
import           Data.Text.Format           (Only (..), format)
import           Data.Text.Lazy             (Text)
import           GHC.TypeLits               (KnownSymbol, symbolVal)
import           Perst.Database.Constraints (DDLConstr)
import           Perst.Database.DataDef     (DataDef, DataDef' (..), fieldNames,
                                             foreignKeys, primaryKey, tableName,
                                             uniqKeys)
import           Perst.Database.DbOption    (DbOption (..), SessionMonad,
                                             dbTypeNames)

class DbOption b => DDL b (t :: DataDef) where
  create :: MonadIO m => Proxy t -> SessionMonad b m ()
  create = execCommand . createText (Proxy :: Proxy b)
  drop :: MonadIO m => Proxy t -> SessionMonad b m ()
  drop   = execCommand . dropText (Proxy :: Proxy b)
  createText :: Proxy b -> Proxy t -> Text
  dropText :: Proxy b -> Proxy t -> Text

instance DDLConstr b (TableDef n r fn p u f) => DDL b (TableDef n r fn p u f) where
  createText pb pt
    = format "CREATE TABLE {} {} ({}, PRIMARY KEY ({}) {} {})"
        ( afterCreateTableText pb
        , tableName pt
        , intercalate ","
            $ zipWith (\n (t,b) -> n ++ " " ++ t
                                  ++ if b then " NULL" else " NOT NULL")
                      (fieldNames pt) (dbTypeNames pb pt)
        , intercalate "," $ primaryKey pt
        , foldMap (format ",UNIQUE ({})" . Only . intercalate ",")
            $ uniqKeys pt
        , foldMap ( format ",FOREIGN KEY ({}) REFERENCES {} ({}) {} "
                  . ((,,,)  <$> intercalate "," . fst . fst
                            <*> fst . snd
                            <*> intercalate "," . snd . fst
                            <*> deleteConstraintText pb . snd . snd
                    )
                  . first unzip
                  ) $ foreignKeys pt
        )
  dropText _ pt = format "DROP TABLE {}" $ Only (tableName pt :: String)

instance (DDLConstr b (ViewDef n r fn (Just s) upd p u f), KnownSymbol s)
        => DDL b (ViewDef n r fn (Just s) upd p u f) where
  createText pb pt
    = format "CREATE VIEW {} {} AS {}"
        ( afterCreateTableText pb
        , tableName pt
        , symbolVal (Proxy :: Proxy s)
        )
  dropText _ pt = format "DROP VIEW {}" $ Only (tableName pt :: String)
