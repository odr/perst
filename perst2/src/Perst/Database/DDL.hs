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
import           Data.Kind               (Type)
import           Data.Singletons.Prelude
import           Data.Text               (Text)
import qualified Data.Text               as T
import           Data.Text.Format        (Only (..))
import           GHC.Prim                (Proxy#, proxy#)
import           Prelude                 hiding (drop)

import           Data.Type.GrecTree      (ConsNames, ConvNames (..), Grec (..))
import           Perst.Database.DataDef  (DataStruct, DataStructInfo (..),
                                          DsRec, GetFromRefs, Ref, Ref' (..),
                                          formatS)
import           Perst.Database.DbOption (DbFieldTypes, DbOption (..),
                                          MonadCons, SessionMonad)
import           Perst.Types             (NoLstFld)

type DDLConsV b t = (DbOption b, DataStructInfo t)
type DDLCons b t =  ( DDLConsV b t, SingI (DbFieldTypes b (DsRec t))
                    , Grec (DsRec t), ConsNames NoLstFld (DsRec t)
                    )

class DDLCons b t => DDL b (t::DataStruct) (rs::[Ref]) where
  type DDLRefs t rs :: [Ref]
  type DDLRefs t rs = GetFromRefs t rs

  create      :: (MonadCons m, SingI (DDLRefs t rs)) => SessionMonad b m ()
  create = execCommand @b (createText @b @t @rs)

  drop        :: MonadCons m => SessionMonad b m ()
  drop   = execCommand @b (dropText @b @t @rs)

  createText  :: SingI (DDLRefs t rs) => Text
  createText
    | isTable @t = createTextTable (proxy# :: Proxy# '(b,t,DDLRefs t rs))

    | otherwise = createTextView (proxy# :: Proxy# '(b,t))

  dropText    :: Text
  dropText = formatS "DROP TABLE {}" $ Only (tableName @t)

  dropCreate :: (MonadCons m, SingI (DDLRefs t rs)) => SessionMonad b m ()
  dropCreate = do
    catch (drop @b @t @rs) (\(_::SomeException) -> return ())
    create @b @t @rs

createTextTable :: (SingI rs, DDLCons b t)
              => Proxy# ('(b,t,rs) :: (Type,DataStruct,[Ref])) -> Text
createTextTable (_ :: Proxy# '(b,t,rs))
  = formatS "CREATE TABLE {} {} ({}, PRIMARY KEY ({}) {} {})"
      ( afterCreateTableText @b
      , tableName @t
      , T.intercalate ","
          $ zipWith (\n (t,b) -> formatS "{} {} {} NULL"
                                          (n,t, if b then T.empty else "NOT"))
                    (fldNames @NoLstFld @(DsRec t))
                    (fromSing (sing :: Sing (DbFieldTypes b (DsRec t))))
      , T.intercalate "," $ primaryKey @t
      , foldMap (formatS ",UNIQUE ({})" . Only . T.intercalate ",")
          $ uniqKeys @t
      , foldMap (formatS ",FOREIGN KEY ({}) REFERENCES {} ({}) {} "
                . ((,,,)  <$> T.intercalate "," . map fst . refCols
                          <*> refTo
                          <*> T.intercalate "," . map snd . refCols
                          <*> deleteConstraintText @b . refDelCons
                  )
                ) $ fromSing (sing :: Sing rs)
      )
createTextView :: DDLConsV b t => Proxy# '(b,t) -> Text
createTextView (_ :: Proxy# '(b,t))
  = formatS "CREATE VIEW {} {} AS {}"
    ( afterCreateTableText @b
    , tableName @t
    , viewText @t
    )
