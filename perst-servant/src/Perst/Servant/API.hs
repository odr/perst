{-# LANGUAGE DataKinds        #-}
-- {-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE MagicHash        #-}
-- {-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeInType       #-}
{-# LANGUAGE TypeOperators    #-}
module Perst.Servant.API where

import           Control.Monad
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Reader (runReaderT)
import           Data.Kind                  (Type)
import           GHC.Prim                   (Proxy#, proxy#)
-- import           Lucid
import           Servant                    ((:<|>) (..), (:>) (..), Capture,
                                             Delete, Get, Handler, JSON,
                                             NoContent (..), Post, Proxy (..),
                                             Put, ReqBody, Server, err404,
                                             throwError)
import           Servant.Client             (Client, ClientM, HasClient, client)
-- import           Servant.HTML.Lucid         (HTML)

import           Data.Type.Grec             (Grec (..), GrecWith (..),
                                             NamesGrecLens, gwTagged)
import           Perst.Database.Condition   (Condition)
import           Perst.Database.DataDef     (DataDef)
import           Perst.Database.DbOption    (DbOption (..), SessionMonad)
import           Perst.Database.DMLTree     (DMLTree (..))
import           Perst.Database.Tree.Select (SelTreeCond, SelTreeCons)
import           Perst.Database.TreeDef     (TopKey, TopPK, TopPKPairs,
                                             TopPKTagged, TreeDef,
                                             TreeDef' (TreeDefC))

import           Perst.Servant.Types        (PerstPar (..), PerstRes (..))

type PK t r = TopPKTagged t (Grec r)
type PPK t r = PerstPar (PK t  r)

type PerstAPI (t :: TreeDef) r
  = "getList" :> ReqBody '[JSON] (Condition t r) :> Post '[JSON] (PerstRes t [r])
  :<|>  Capture "pk" (PPK t r)                   :> Get '[JSON] (PerstRes t r)
  :<|>  ReqBody '[JSON] r                        :> Post '[JSON] (PK t r)
  :<|>  ReqBody '[JSON] r                        :> Put '[JSON] NoContent
  :<|>  "diff" :> ReqBody '[JSON] (r, r)         :> Put '[JSON] NoContent
  :<|>  Capture "pk" (PPK t r)                   :> Delete '[JSON] NoContent

serverPerstAPI :: (DMLTree b t r, SelTreeCond b t r, SelTreeCons b t () r
                  , NamesGrecLens (TopKey t) (TopPKPairs t (Grec r)) (Grec r)
                  )
               => Proxy# '(b,t,r) -> Conn b -> Server (PerstAPI t r)
serverPerstAPI (_ :: Proxy# '(b,t,r)) conn
    = getList :<|> getRec :<|> ins :<|> upd :<|> updDiff :<|> del
  where
    runSess :: forall a. SessionMonad b IO a -> Handler a
    runSess r = liftIO $ runReaderT r conn

    check404 rs = if null rs then throwError err404 else return $ head rs

    getPK (r'::r) = gwTagged (GW (Grec r') :: TopPK t (Grec r))

    getList :: Condition t r -> Handler (PerstRes t [r])
    getList = fmap PerstRes . runSess . selectTreeCond @b @t @r

    getRec  :: PPK t r -> Handler (PerstRes t r)
    getRec (PerstPar pk) = PerstRes <$> getRec' pk

    getRec' :: PK t r -> Handler r
    getRec' pk = runSess (concat <$> selectTreeMany @b @t @r [pk]) >>= check404

    ins     :: r -> Handler (PK t r)
    ins r = runSess (map getPK <$> insertTreeMany @b @t [r]) >>= check404

    upd     :: r -> Handler NoContent
    upd r = (,r) <$> getRec' (getPK r) >>= updDiff

    updDiff :: (r,r) -> Handler NoContent
    updDiff (old,new) = runSess (updateTreeMany @b @t [old] [new])
                      >> return NoContent

    del     :: PPK t r -> Handler NoContent
    del (PerstPar pk)
      = (:[]) <$> getRec' pk >>= runSess . deleteTreeMany @b @t @r
      >> return NoContent

perstAPI :: Proxy '(t,r) -> Proxy (PerstAPI t r)
perstAPI _ = Proxy

perstClient :: HasClient (PerstAPI t r) => Proxy '(t,r) -> Client (PerstAPI t r)
perstClient = client . perstAPI

getList :: Client (PerstAPI t r) -> Condition t r -> ClientM (PerstRes t [r])
getList (c :<|> _) = c
getRec :: Client (PerstAPI t r) -> PPK t r -> ClientM (PerstRes t r)
getRec (_ :<|> c :<|> _) = c
insRec :: Client (PerstAPI t r) -> r -> ClientM (PK t r)
insRec (_ :<|> _ :<|> c :<|> _) = c
updRec :: Client (PerstAPI t r) -> r -> ClientM NoContent
updRec (_ :<|> _ :<|> _ :<|> c :<|> _) = c
updDiffRec :: Client (PerstAPI t r) -> (r,r) -> ClientM NoContent
updDiffRec (_ :<|> _ :<|> _ :<|> _ :<|> c :<|> _) = c
delRec :: Client (PerstAPI t r) -> PPK t r -> ClientM NoContent
delRec (_ :<|> _ :<|> _ :<|> _ :<|> _ :<|> c) = c
