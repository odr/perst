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
import           Data.Singletons.Prelude   (Head, If)
import           Data.Tagged               (Tagged (..))
import           GHC.Prim                  (Proxy#, proxy#)

import           Data.Type.GrecTree        (ConvNames (..), Grec (..),
                                            TGetSet (..), TShowHide (..))
import           Perst.Database.DataDef    (DdAutoIns, DdKey, GetDS,
                                            GetDataStruct, GetToRefByName,
                                            RefCols, RefFrom, SchRefs)
import           Perst.Database.DbOption   (AppCons, MonadCons, SessionMonad)
import           Perst.Database.DML.Insert (InsCons, ReturnInsKey (..),
                                            insertManyDef)
import           Perst.Types               (Fsts, LstFld, PChilds (..), Snds)

type RKType t b = ReturnKey (DdAutoIns t) b
type RKName t = If (DdAutoIns t) (Head (DdKey t)) ""
type RK t b = Tagged (RKName t) (RKType t b)

type InsTreeCons b sch t r =
  ( InsCons b (GetDS t sch) r
  , InsertChilds b sch t (FldNames LstFld r) r
  )

insertTreeManyDef :: (AppCons f, MonadCons m
                     , GetDataStruct t sch ~ Just ds
                     , InsTreeCons b sch t r
                     )
  => Proxy# '(b,sch,t) -> f r -> SessionMonad b m (f r)
insertTreeManyDef (_ :: Proxy# '(b,sch,t)) (rs :: f r) = do
  -- undefined
  fk <- insertManyDef (proxy# :: Proxy# '(b,GetDS t sch)) rs
  insertChilds @b @sch @t @(FldNames LstFld r) fk rs

class InsertChilds b sch t fs r where
  insertChilds  :: (MonadCons m, AppCons f)
                => f (RKType (GetDS t sch) b) -> f r -> SessionMonad b m (f r)

instance InsertChilds b sch t '[] r where
  insertChilds _ = return

type InsChildRec b td ref r v =
  ( Tagged (Fsts ref) (GSType (Snds ref) (RK td b,r))
  , GrecTagged v
  )
instance ( GetToRefByName t s (SchRefs sch) ~ Just ref
         , GSType '[s] r ~ PChilds v
         , TGetSet '[s] r
         , Grec v, GrecTagged v ~ tv
         , RefCols ref ~ rcols
         , Fsts rcols ~ rcols1
         , Snds rcols ~ rcols2
         , GetDataStruct (RefFrom ref) sch ~ Just cds
         , GetDataStruct t sch ~ Just pds
         , TShowHide (RKName pds) r, dr ~ HiddenType (RKName pds) r
         , (RK pds b,dr) ~ prnt
         , Grec prnt, GrecTagged prnt ~ tprnt
         , TGetSet rcols2 tprnt, GSType rcols2 tprnt ~ vref
         , Tagged rcols1 vref ~ chfk
         , TShowHide rcols1 tv
         , dtv ~ HiddenType rcols1 tv
         , Grec (chfk,dtv), GrecTagged (chfk, dtv) ~ tchld
         , InsTreeCons b sch (RefFrom ref) tchld
         , InsertChilds b sch t ss r
         , Show (RKType pds b, r)
         , Show tchld
         )
      => InsertChilds b sch t (s ': ss) r where
  insertChilds ks rs = do
    -- undefined
    let xs = Compose $ newRec <$> ks <*> rs
    liftIO $ do
      putStrLn "\ninsertChilds:"
      mapM_ print $ (,) <$> ks <*> rs
      mapM_ print xs
    -- rs' <- fmap ( liftA2 (\r r' -> r & (tlens' (sing :: Sing s)) .~ r') rs
    rs' <- fmap ( liftA2 (flip (tset @'[s])) rs
                . fmap ( PChilds
                       . map ( fromTagged
                             . tshow @rcols1
                             . snd
                             . fromTagged @(chfk, dtv))
                       . getZipList
                       )
                . getCompose
                )
        $ insertTreeManyDef (proxy# :: Proxy# '(b,sch,RefFrom ref))
        $ Compose $ newRec <$> ks <*> rs
    insertChilds @b @sch @t @ss ks rs'
   where
    newRec k r
      = ( toTagged
        . ( Tagged @rcols1
            (tget @rcols2 $ toTagged @prnt (Tagged k, thide @(RKName pds) r)), )
        . thide @rcols1
        )
      <$> ZipList (map toTagged $ unPChilds $ tget @'[s] r) -- $ view (tlens' (sing :: Sing s)) r)
