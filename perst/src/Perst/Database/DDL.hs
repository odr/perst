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
-- import qualified Data.Text.Lazy         as TL
import           GHC.TypeLits               (KnownSymbol, symbolVal)
-- import           Data.Singletons.Prelude (Sing (..))
import           Data.Type.Grec
import           Perst.Database.Constraints
import           Perst.Database.Types
import           Perst.Types

class DBOption b => DDL b (t :: DataDef) where
  create :: MonadIO m => Proxy t -> SessionMonad b m ()
  create = execCommand . createText (Proxy :: Proxy b)
  drop :: MonadIO m => Proxy t -> SessionMonad b m ()
  drop   = execCommand . dropText (Proxy :: Proxy b)
  createText :: Proxy b -> Proxy t -> Text
  dropText :: Proxy b -> Proxy t -> Text

instance DDLConstr b (TableDef n r p u f) => DDL b (TableDef n r p u f) where
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

instance (DDLConstr b (ViewDef n r (Just s) upd p u f), KnownSymbol s)
        => DDL b (ViewDef n r (Just s) upd p u f) where
  createText pb pt
    = format "CREATE VIEW {} {} AS {}"
        ( afterCreateTableText pb
        , tableName pt
        , symbolVal (Proxy :: Proxy s)
        )
  dropText _ pt = format "DROP VIEW {}" $ Only (tableName pt :: String)
{-
createTableText :: TabConstrB b t => Proxy b -> Proxy t -> Text
createTableText pb pt
  = format "CREATE TABLE {} {} ({}, PRIMARY KEY ({}) {} {})"
      ( afterCreateTableText pb
      , tableName pt
      , intercalate ","
          $ zipWith (\n (t,b) -> n ++ " " ++ t ++ if b then " NULL" else " NOT NULL")
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
dropTableText :: TabConstr t => Proxy t -> Text
dropTableText pt = format "DROP TABLE {}" $ Only (tableName pt :: String)

createTable :: (TabConstrB b t, MonadIO m)
            => Proxy b -> Proxy t -> SessionMonad b m ()
createTable pb = execCommand . createTableText pb

dropTable :: (TabConstrB b t, MonadIO m) => Proxy t -> SessionMonad b m ()
dropTable = execCommand . dropTableText
-}
