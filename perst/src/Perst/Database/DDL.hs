module Perst.Database.DDL
    ( createTableText
    , dropTableText
    , createTable
    , dropTable
    ) where

import           Control.Arrow          (first)
import           Control.Monad.IO.Class (MonadIO)
import           Data.Kind              (Type)
import           Data.List              (intercalate)
import           Data.Proxy             (Proxy (..))
import           Data.Text.Format       (Only (..), format)
import           Data.Text.Lazy         (Text)
-- import qualified Data.Text.Lazy         as TL
import           Data.Type.Grec
import           Perst.Database.Types
import           Perst.Types


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