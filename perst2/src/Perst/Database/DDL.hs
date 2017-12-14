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

import           Control.Monad.Catch     (SomeException, catch)
import           Data.Singletons.Prelude
import           Data.Text               (Text)
import qualified Data.Text               as T
import           Data.Text.Format        (Only (..))
import           GHC.Prim                (Proxy#, proxy#)
import           Prelude                 hiding (drop)

import           Data.Type.GrecTree      (ConsNames, ConvNames (..), Grec (..))
import           Perst.Database.DataDef  (DataDefInfo (..), FK (..), formatS)
import           Perst.Database.DbOption (DbFieldTypes, DbOption (..),
                                          MonadCons, SessionMonad)
import           Perst.Types             (NoLstFld)

type DDLConsV b t = (DbOption b, DataDefInfo t)
type DDLCons b t r = (DDLConsV b t, SingI (DbFieldTypes b r), Grec r, ConsNames NoLstFld r)

-- type Rec r = FieldsConvGrec (Grec r)

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
                    (fldNames @NoLstFld @r) (fromSing (sing :: Sing (DbFieldTypes b r)))
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
