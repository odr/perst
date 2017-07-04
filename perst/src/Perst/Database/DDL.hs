{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}
module Perst.Database.DDL
    ( DDL (..)
    ) where

import           Control.Arrow              (first)
import           Control.Monad              ((>=>))
import           Control.Monad.IO.Class     (MonadIO)
import           Data.Kind                  (Type)
import           Data.List                  (intercalate)
import           Data.Proxy                 (Proxy (..))
import           Data.Text.Format           (Only (..), format)
import           Data.Text.Lazy             (Text)
import           GHC.TypeLits               (KnownSymbol, symbolVal)
import           Perst.Database.Constraints (DDLConstr)
import           Perst.Database.DataDef     (DataDef, DataDef' (..),
                                             DataDef'' (..), fieldNames,
                                             foreignKeys, primaryKey, tableName,
                                             uniqKeys)
import           Perst.Database.DbOption    (DbOption (..), SessionMonad,
                                             dbTypeNames)

class DDLConstr m b t => DDL m b (t :: DataDef) where
  create      :: Proxy t -> SessionMonad b m ()
  drop        :: Proxy t -> SessionMonad b m ()
  createText  :: Proxy b -> Proxy t -> SessionMonad b m Text
  dropText    :: Proxy b -> Proxy t -> SessionMonad b m Text
  create = createText (Proxy :: Proxy b) >=> execCommand
  drop   = dropText (Proxy :: Proxy b) >=> execCommand

instance DDLConstr m b (DataDefC (TableDef n r fn p u ai) f)
      => DDL m b (DataDefC (TableDef n r fn p u ai) f) where
  createText pb pt
    = return $ format "CREATE TABLE {} {} ({}, PRIMARY KEY ({}) {} {})"
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
  dropText _ pt = return $ format "DROP TABLE {}" $ Only (tableName pt)

instance (DDLConstr m b (DataDefC (ViewDef n r fn (Just s) upd p u ai) f), KnownSymbol s)
      => DDL m b (DataDefC (ViewDef n r fn (Just s) upd p u ai) f) where
  createText pb pt
    = return $ format "CREATE VIEW {} {} AS {}"
        ( afterCreateTableText pb
        , tableName pt
        , symbolVal (Proxy :: Proxy s)
        )
  dropText _ pt = return $ format "DROP VIEW {}" $ Only (tableName pt)
