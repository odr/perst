{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}
module Perst.Database.DML.Insert where

import           Control.Monad                (when)
import           Control.Monad.Catch          (finally)
import           Data.List                    ((\\))
import           Data.Singletons.Prelude.List (Sort)
import qualified Data.Text                    as T
import           GHC.Prim                     (Proxy#, proxy#)

import           Data.Type.GrecTree           (ConsNames, ConvNames (..),
                                               Convert (..), SubsetNamesTypes,
                                               TShowHide (..))
import           Perst.Database.DataDef       (DataStructInfo (..), DdAutoIns,
                                               DdKey, DsRec, formatS)
import           Perst.Database.DbOption      (DbOption (..), MonadCons,
                                               SessionMonad)
import           Perst.Types                  (MandatoryFld, NoLstFld)

type InsTextCons b t r = (DbOption b, DataStructInfo t, ConsNames NoLstFld r)

type InsPlusCons t r =
  ( Sort (FldNames MandatoryFld r) ~ Sort (FldNames MandatoryFld (DsRec t))
  , SubsetNamesTypes NoLstFld r (DsRec t)
  )
type InsCons b t r =
  (InsTextCons b t r, ReturnInsKey (DdAutoIns t) b t r, InsPlusCons t r)

insertTextDef :: InsTextCons b t r => Proxy# '(b,t,r) -> T.Text
insertTextDef (_ :: Proxy# '(b,t,r))
  = formatS "INSERT INTO {}({}) VALUES({})"
  ( tableName @t
  , T.intercalate "," fns
  , T.intercalate "," $ zipWith (const $ paramName @b) fns [0..]
  )
 where
  fns = fldNames @NoLstFld @r \\ if autoIns @t then primaryKey @t else []

class ReturnInsKey ai b t r where
  type ReturnKey ai b
  convertRec :: r -> [FieldDB b]
  returnKey :: MonadCons m => SessionMonad b m (ReturnKey ai b)

instance Convert r [FieldDB b] => ReturnInsKey False b t r where
  type ReturnKey False b = ()
  convertRec = convert
  returnKey = return ()

instance ( DbOption b
         , TShowHide (DdKey t) r
         , Convert (HiddenType (DdKey t) r) [FieldDB b]
         -- , Convert r [FieldDB b]
         )
      => ReturnInsKey True b t r where
  type ReturnKey True b = GenKey b
  convertRec = convert . thide @(DdKey t)
  returnKey = getLastKey @b

insertManyDef :: (MonadCons m, Traversable f, InsCons b t r)
    => Proxy# '(b,t) -> f r -> SessionMonad b m (f (ReturnKey (DdAutoIns t) b))
insertManyDef (_ :: Proxy# '(b,t)) (rs :: f r)
  | null rs = mapM (\_ -> returnKey @(DdAutoIns t) @b @t @r) rs
  | otherwise = do
    when ai $ preRunInAuto @b
    (cmd :: PrepCmd b) <- prepareCommand @b
                        $ insertTextDef (proxy# :: Proxy# '(b,t,r))
    finally ( mapM (\r -> do
                    runPrepared @b cmd r
                    returnKey @(DdAutoIns t) @b @t @r
                ) $ fmap (convertRec @(DdAutoIns t) @b @t) rs
            )
            (finalizePrepared @b cmd)
 where
  ai = autoIns @t
