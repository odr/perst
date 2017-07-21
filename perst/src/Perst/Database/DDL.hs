{-# LANGUAGE RankNTypes           #-}
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
import           Data.Singletons.Prelude    (Sing)
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
  create      :: Sing t -> SessionMonad b m ()
  drop        :: Sing t -> SessionMonad b m ()
  createText  :: Proxy b -> Sing t -> SessionMonad b m Text
  dropText    :: Proxy b -> Sing t -> SessionMonad b m Text
  create pt = createText (Proxy :: Proxy b) pt >>= execCommand
  drop   pt = dropText   (Proxy :: Proxy b) pt >>= execCommand

instance DDLConstr m b (DataDefC (TableDef n r fn p u ai) f)
      => DDL m b (DataDefC (TableDef n r fn p u ai) f) where
  createText pb st
    = return $ format "CREATE TABLE {} {} ({}, PRIMARY KEY ({}) {} {})"
        ( afterCreateTableText pb
        , tableName st
        , intercalate ","
            $ zipWith (\n (t,b) -> n ++ " " ++ t
                                  ++ if b then " NULL" else " NOT NULL")
                      (fieldNames st) (dbTypeNames pb st)
        , intercalate "," $ primaryKey st
        , foldMap (format ",UNIQUE ({})" . Only . intercalate ",")
            $ uniqKeys st
        , foldMap ( format ",FOREIGN KEY ({}) REFERENCES {} ({}) {} "
                  . ((,,,)  <$> intercalate "," . fst . fst
                            <*> fst . snd
                            <*> intercalate "," . snd . fst
                            <*> deleteConstraintText pb . snd . snd
                    )
                  . first unzip
                  ) $ foreignKeys st
        )
  dropText _ pt = return $ format "DROP TABLE {}" $ Only (tableName pt)

instance (DDLConstr m b (DataDefC (ViewDef n r fn (Just s) upd p u ai) f), KnownSymbol s)
      => DDL m b (DataDefC (ViewDef n r fn (Just s) upd p u ai) f) where
  createText pb st
    = return $ format "CREATE VIEW {} {} AS {}"
        ( afterCreateTableText pb
        , tableName st
        , symbolVal (Proxy :: Proxy s)
        )
  dropText _ st = return $ format "DROP VIEW {}" $ Only (tableName st)
