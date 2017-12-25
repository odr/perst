{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}
module Perst.Database.Tree.Delete where

import           Control.Applicative       (ZipList (..))
-- import           Control.Monad.IO.Class    (MonadIO (..))
import           Data.Functor.Compose      (Compose (..))
import           Data.Tagged               (Tagged (..))
import           GHC.Prim                  (Proxy#, proxy#)
-- import           Lens.Micro.Extras         (view)

import           Data.Type.GrecTree        (ConvNames (..), Grec (..),
                                            TGetSet (..), TShowHide (..))
import           Perst.Database.DataDef    (GetDS, GetDataStruct,
                                            GetToRefByName, RefCols, RefFrom,
                                            SchRefs)
import           Perst.Database.DbOption   (AppCons, MonadCons, SessionMonad)
import           Perst.Database.DML.Delete (DelCons, deleteManyDef)
import           Perst.Types               (Fsts, LstFld, PChilds (..), Snds)

type DelTreeCons b sch t r =
  (DelCons b (GetDS t sch) r, DeleteChilds b sch t (FldNames LstFld r) r)

deleteTreeManyDef :: (MonadCons m , AppCons f
                     , GetDataStruct t sch ~ Just ds
                     , DelTreeCons b sch t r
                     )
  => Proxy# '(b,sch,t) -> f r -> SessionMonad b m ()
deleteTreeManyDef (_ :: Proxy# '(b,sch,t)) (rs :: f r) = do
  deleteChilds @b @sch @t @(FldNames LstFld r) rs
  deleteManyDef (proxy#::Proxy# '(b,GetDS t sch)) rs

class DeleteChilds b sch t fs r where
  deleteChilds  :: (MonadCons m , AppCons f) => f r -> SessionMonad b m ()

instance DeleteChilds b sch t '[] r where
  deleteChilds _ = return ()

instance ( GetToRefByName t s (SchRefs sch) ~ Just ref
         , GSType '[s] r ~ PChilds v
         , TGetSet '[s] r
         , Grec v, GrecTagged v ~ tv
         , RefCols ref ~ rcols
         , Fsts rcols ~ rcols1
         , Snds rcols ~ rcols2
         , TGetSet rcols2 r, GSType rcols2 r ~ vref
         , Tagged rcols1 vref ~ chfk
         , TShowHide rcols1 tv
         , dtv ~ HiddenType rcols1 tv
         , Grec (chfk, dtv), GrecTagged (chfk, dtv) ~ tchld
         , GetDataStruct (RefFrom ref) sch ~ Just cds
         , DelTreeCons b sch (RefFrom ref) tchld
         , DeleteChilds b sch t ss r
         -- , Show vref, Show dtv
         )
      => DeleteChilds b sch t (s ': ss) r where
  deleteChilds rs = do
    -- liftIO $ do
    --   putStrLn "deleteChilds:"
    --   mapM_ print $ Compose $ delRec <$> rs
    deleteTreeManyDef (proxy# :: Proxy# '(b,sch,(RefFrom ref)))
                    $ Compose $ delRec <$> rs
    deleteChilds @b @sch @t @ss rs
    where
      delRec r = ( toTagged
                 . (Tagged @rcols1 (tget @rcols2 r),)
                 . thide @rcols1
                 )
              <$> ZipList (map toTagged $ unPChilds $ tget @'[s] r)
