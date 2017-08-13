{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}
module Perst.Database.DDL
    ( DDL (..)
    ) where

import           Control.Arrow           (first)
import           Control.Monad           ((>=>))
import           Control.Monad.Catch     (SomeException, catch)
import           Control.Monad.IO.Class  (MonadIO)
import           Data.Kind               (Type)
import           Data.List               (intercalate)
import           Prelude                 hiding (drop)
-- import           Data.Proxy              (Proxy (..))
import           Data.Singletons.Prelude (Fst)
import           Data.Text               (Text)
import qualified Data.Text               as T
import           Data.Text.Format        (Only (..))
import           GHC.Prim                (Proxy#, proxy#)
import           GHC.TypeLits            (KnownSymbol, symbolVal)
-- import           Perst.Database.Constraints (DDLConstr)

import           Data.Type.Grec          (Grec)
import           Perst.Database.DataDef  (DataDef, DataDef' (..),
                                          DataDefInfo (..), DataInfo (..),
                                          FK (..), formatS)
import           Perst.Database.DbOption (DbOption (..), DbTypeNames (..),
                                          MonadCons, SessionMonad)

type DDLConsV b t = (DbOption b, DataDefInfo t)
type DDLCons b t = (DDLConsV b t, DbTypeNames b (Grec (Fst t)))

class DDLConsV b t => DDL b t where
  create      :: MonadCons m => SessionMonad b m ()
  drop        :: MonadCons m => SessionMonad b m ()
  createText  :: Text
  dropText    :: Text
  dropCreate :: MonadCons m => SessionMonad b m ()
  dropCreate = do
    catch (drop @b @t) (\(_::SomeException) -> return ())
    create @b @t

  dropText = formatS "DROP TABLE {}" $ Only (tableName @t)
  create = execCommand @b (createText @b @t)
  drop   = execCommand @b (dropText   @b @t)


instance DDLCons b '(r, DataDefC (TableInfo p u ai) f)
    => DDL b '(r, DataDefC (TableInfo p u ai) f) where
  createText = createTextTable
      (proxy# :: Proxy# b)
      (proxy# :: Proxy# '(r, DataDefC (TableInfo p u ai) f))

instance DDLConsV b '(r, DataDefC (ViewInfo (Just s) upd p u ai) f)
      => DDL b '(r, DataDefC (ViewInfo (Just s) upd p u ai) f) where
  createText = createTextView
      (proxy# :: Proxy# b)
      (proxy# :: Proxy# '(r, DataDefC (ViewInfo (Just s) upd p u ai) f))

createTextTable :: DDLCons b t => Proxy# b -> Proxy# t -> Text
createTextTable (_ :: Proxy# b) (_ :: Proxy# t)
  = formatS "CREATE TABLE {} {} ({}, PRIMARY KEY ({}) {} {})"
      ( afterCreateTableText @b
      , tableName @t
      , T.intercalate ","
          $ zipWith (\n (t,b) -> formatS "{} {} {} NULL"
                                          (n,t, if b then T.empty else "NOT"))
                    (fieldNames @t) (dbTypeNames @b @(Grec (Fst t)))
      , T.intercalate "," $ primaryKey @t
      , foldMap (formatS ",UNIQUE ({})" . Only . T.intercalate ",")
          $ uniqKeys @t
      , foldMap (formatS ",FOREIGN KEY ({}) REFERENCES {} ({}) {} "
                . ((,,,)  <$> T.intercalate "," . map fst . fkRefs
                          <*> fkRefTab
                          <*> T.intercalate "," . map snd . fkRefs
                          <*> deleteConstraintText @b . fkDelCons
                  )
                ) $ foreignKeys @t
      )
createTextView :: DDLConsV b t => Proxy# b -> Proxy# t -> Text
createTextView (_ :: Proxy# b) (_ :: Proxy# t)
  = formatS "CREATE VIEW {} {} AS {}"
    ( afterCreateTableText @b
    , tableName @t
    , viewText @t
    )
