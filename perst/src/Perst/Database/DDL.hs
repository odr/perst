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

import           Data.Type.Grec          (ConvGrecInfo (..), FieldsConvGrec,
                                          Grec)
import           Perst.Database.DataDef  (DataDef, DataDef' (..),
                                          DataDefInfo (..), DataInfo (..),
                                          FK (..), formatS)
import           Perst.Database.DbOption (DbOption (..), DbTypeNames (..),
                                          MonadCons, SessionMonad)

type DDLConsV b t = (DbOption b, DataDefInfo t)
type DDLCons b t r = (DDLConsV b t, DbTypeNames b (Rec r), ConvGrecInfo (Grec r))

type Rec r = FieldsConvGrec (Grec r)

class DDLCons b t r => DDL b t r where
  create      :: MonadCons m => SessionMonad b m ()
  create = execCommand @b (createText @b @t @r)

  drop        :: MonadCons m => SessionMonad b m ()
  drop   = execCommand @b (dropText   @b @t @r)

  createText  :: Text
  createText
    | isTable @t = createTextTable (proxy# :: Proxy# '(b,t,r))

    | otherwise = createTextView (proxy# :: Proxy# '(b,t))

  dropText    :: Text
  dropText = formatS "DROP TABLE {}" $ Only (tableName @t)

  dropCreate :: MonadCons m => SessionMonad b m ()
  dropCreate = do
    catch (drop @b @t @r) (\(_::SomeException) -> return ())
    create @b @t @r

createTextTable :: DDLCons b t r => Proxy# '(b,t,r) -> Text
createTextTable (_ :: Proxy# '(b,t,r))
  = formatS "CREATE TABLE {} {} ({}, PRIMARY KEY ({}) {} {})"
      ( afterCreateTableText @b
      , tableName @t
      , T.intercalate ","
          $ zipWith (\n (t,b) -> formatS "{} {} {} NULL"
                                          (n,t, if b then T.empty else "NOT"))
                    (fieldNames @(Grec r)) (dbTypeNames @b @(Rec r))
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
createTextView :: DDLConsV b t => Proxy# '(b,t) -> Text
createTextView (_ :: Proxy# '(b,t))
  = formatS "CREATE VIEW {} {} AS {}"
    ( afterCreateTableText @b
    , tableName @t
    , viewText @t
    )
