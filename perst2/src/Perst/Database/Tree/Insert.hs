{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}
module Perst.Database.Tree.Insert where

import           Control.Applicative       (ZipList (..), liftA2)
import           Control.Monad.IO.Class    (MonadIO (..))
import           Data.Functor.Compose      (Compose (..))
import           Data.Kind                 (Type)
import           Data.Singletons.Prelude   (Head, If)
import           Data.Tagged               (Tagged (..))
import           GHC.Prim                  (Proxy#, proxy#)
import           Lens.Micro                ((&), (.~))
import           Lens.Micro.Extras         (view)

import           Data.Type.GrecTree        (ConvNames (..), Grec (..), LType,
                                            TGetSet (..), TLens' (..))
import           Perst.Database.DataDef    (DdAutoIns, DdKey)
import           Perst.Database.DbOption   (MonadCons, SessionMonad)
import           Perst.Database.DML.Insert (InsCons, ReturnInsKey (..),
                                            insertManyDef)
import           Perst.Database.TreeDef    (AppCons, MbChild, TdData, TreeDef)
import           Perst.Types               (Fsts, LstFld, PChilds (..), Snds)

type RKType t b = ReturnKey (DdAutoIns t) b
type RKName t = If (DdAutoIns t) (Head (DdKey t)) ""
type RK t b = Tagged (RKName t) (RKType t b)

type InsTreeCons b t r =
  ( InsCons b (TdData t) r
  , InsertChilds b t (FldNames LstFld r) r
  )

insertTreeManyDef :: (AppCons f, MonadCons m, InsTreeCons b t r)
  => Proxy# ('(b,t) :: (Type,TreeDef)) -> f r -> SessionMonad b m (f r)
insertTreeManyDef (_ :: Proxy# '(b,t)) (rs :: f r) = do
  fk <- insertManyDef (proxy# :: Proxy# '(b,TdData t)) rs
  insertChilds @b @t @(FldNames LstFld r) fk rs

class InsertChilds b (t::TreeDef) fs r where
  insertChilds  :: (MonadCons m, AppCons f)
                => f (RKType (TdData t) b) -> f r -> SessionMonad b m (f r)

instance InsertChilds b t '[] r where
  insertChilds _ = return

type InsChildRec b td ref r v =
  ( Tagged (Fsts ref) (GSType (Snds ref) (RK td b,r))
  , GrecTagged v
  )
instance ( MbChild s t ~ Just '(td,ref)
         , LType s r ~ PChilds v
         , TLens' s r
         , Grec v, GrecTagged v ~ tv
         , (RK (TdData t) b,r) ~ prnt
         , Grec prnt, GrecTagged prnt ~ tprnt
         , Snds ref ~ pref
         , TGetSet pref tprnt, GSType pref tprnt ~ vref
         , Fsts ref ~ cref
         , Tagged cref vref ~ chfk
         , Grec (chfk, tv), GrecTagged (chfk, tv) ~ tchld
         , InsTreeCons b td tchld
         , InsertChilds b t ss r
         , Show (RKType (TdData t) b, r)
         , Show tchld
         )
      => InsertChilds b t (s ': ss) r where
  insertChilds ks rs = do
    let xs = Compose $ newRec <$> ks <*> rs
    liftIO $ do
      putStrLn "\ninsertChilds:"
      mapM_ print $ (,) <$> ks <*> rs
      mapM_ print xs
    rs' <- fmap ( liftA2 (\r r' -> r & tlens' @s .~ r') rs
                . fmap ( PChilds
                       . map (fromTagged . snd . fromTagged @(chfk, tv))
                       . getZipList
                       )
                . getCompose
                )
        $ insertTreeManyDef (proxy# :: Proxy# '(b,td))
        $ Compose $ newRec <$> ks <*> rs
    insertChilds @b @t @ss ks rs'
   where
    newRec k r
      = ( toTagged
        . (Tagged @cref (tget @pref $ toTagged @prnt (Tagged k, r)), )
        )
      <$> ZipList (map toTagged $ unPChilds $ view (tlens' @s) r)
