{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}
module Perst.Database.Tree.Delete where

import           Control.Applicative       (ZipList (..))
import           Control.Monad.IO.Class    (MonadIO (..))
import           Data.Functor.Compose      (Compose (..))
import           Data.Kind                 (Type)
import           Data.Tagged               (Tagged (..))
import           GHC.Prim                  (Proxy#, proxy#)
import           Lens.Micro.Extras         (view)

import           Data.Type.GrecTree        (ConvNames (..), Grec (..), LType,
                                            TGetSet (..), TLens' (..))
import           Perst.Database.DbOption   (MonadCons, SessionMonad)
import           Perst.Database.DML.Delete (DelCons, deleteManyDef)
import           Perst.Database.TreeDef    (AppCons, MbChild, TdData, TreeDef)
import           Perst.Types               (Fsts, LstFld, PChilds (..), Snds)

type DelTreeCons b t r
  = (DelCons b (TdData t) r, DeleteChilds b t (FldNames LstFld r) r)

deleteTreeManyDef :: (MonadCons m , AppCons f, DelTreeCons b t r)
  => Proxy# ('(b,t) :: (Type,TreeDef)) -> f r -> SessionMonad b m ()
deleteTreeManyDef (_ :: Proxy# '(b,t)) (rs :: f r) = do
  deleteChilds @b @t @(FldNames LstFld r) rs
  deleteManyDef (proxy#::Proxy# '(b,TdData t)) rs

class DeleteChilds b (t::TreeDef) fs r where
  deleteChilds  :: (MonadCons m , AppCons f) => f r -> SessionMonad b m ()

instance DeleteChilds b t '[] r where
  deleteChilds _ = return ()

instance ( MbChild s t ~ Just '(td,ref)
         , TLens' s r
         , LType s r ~ PChilds v
         , Grec v, GrecTagged v ~ tv
         , TGetSet (Snds ref) r, GSType (Snds ref) r ~ vref
         , Tagged (Fsts ref) vref ~ chfk
         , Grec (chfk, tv), GrecTagged (chfk, tv) ~ tchld
         , DelTreeCons b td tchld
         , DeleteChilds b t ss r
         , Show vref, Show tv
         )
      => DeleteChilds b t (s ': ss) r where
  deleteChilds rs = do
    liftIO $ do
      putStrLn "deleteChilds:"
      mapM_ print $ Compose $ delRec <$> rs
    deleteTreeManyDef (proxy# :: Proxy# '(b,td))
                    $ fmap toTagged $ Compose $ delRec <$> rs
    deleteChilds @b @t @ss rs
    where
      delRec r = (Tagged @(Fsts ref) (tget @(Snds ref) r),)
              <$> ZipList (map toTagged $ unPChilds $ view (tlens' @s) r)
