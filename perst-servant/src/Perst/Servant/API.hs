{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MagicHash                 #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE TypeOperators             #-}
module Perst.Servant.API where

import           Control.Monad
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Reader (runReaderT)
import           Data.Kind                  (Type)
import           Data.Singletons.Prelude    (Symbol)
import           Data.Singletons.TH         (singletons)
import           Data.Tagged                (Tagged, untag)
import           GHC.Prim                   (Proxy#, proxy#)
import           Servant
-- import           Servant.API
import           Servant.Client

import           Data.Type.Grec             (Grec (..), GrecWith (..),
                                             NamesGrecLens, gwTagged)
import           Perst.Database.Condition   (Condition)
import           Perst.Database.DataDef     (DataDef)
import           Perst.Database.DbOption
import           Perst.Database.DMLTree
import           Perst.Database.Tree.Select (SelTreeCond, SelTreeCons)
import           Perst.Database.TreeDef     (TopKey, TopPK, TopPKPairs,
                                             TopPKTagged, TreeDef,
                                             TreeDef' (TreeDefC))

-- singletons [d|
--   data ServantDef' s = ServantDefC
--     { sdName :: s
--     }
--   |]
-- type ServantDef = ServantDef' Symbol

-- NB orphan instances!!
instance FromHttpApiData v => FromHttpApiData (Tagged x v) where
  parseUrlPiece = fmap Tagged . parseUrlPiece
instance ToHttpApiData v => ToHttpApiData (Tagged x v) where
  toUrlPiece = toUrlPiece . untag

type PK t r = TopPKTagged t (Grec r)

type PerstAPI (t :: TreeDef) r
  =     ReqBody '[JSON] (Condition t r)  :> Get '[JSON] [r]
  :<|>  Capture "pk" (PK t r)            :> Get '[JSON] r
  :<|>  ReqBody '[JSON] r                :> Post '[JSON] (PK t r)
  :<|>  ReqBody '[JSON] r                :> Put '[JSON] NoContent
  :<|>  "diff" :> ReqBody '[JSON] (r, r) :> Put '[JSON] NoContent
  :<|>  Capture "pk" (PK t r)            :> Delete '[JSON] NoContent

-- sessionToHandler :: Proxy# b -> Conn b -> SessionMonad b IO :~> Handler
-- sessionToHandler (_ :: Proxy# b) conn = NT $ \r -> liftIO $ runReaderT r conn

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

    getList :: Condition t r -> Handler [r]
    getList cond = runSess $ selectTreeCond @b @t @r cond

    getRec  :: PK t r -> Handler r
    getRec pk = runSess (concat <$> selectTreeMany @b @t @r [pk]) >>= check404

    ins     :: r -> Handler (PK t r)
    ins r = runSess (map getPK <$> insertTreeMany @b @t [r]) >>= check404

    upd     :: r -> Handler NoContent
    upd r = getRec (getPK r) >>= \old -> updDiff (old,r)

    updDiff :: (r,r) -> Handler NoContent
    updDiff (old,new) = runSess (updateTreeMany @b @t [old] [new])
                      >> return NoContent

    del     :: PK t r -> Handler NoContent
    del pk  = getRec pk
            >>= \old -> runSess (deleteTreeMany @b @t @r [old])
            >> return NoContent

perstAPI :: Proxy '(t,r) -> Proxy (PerstAPI t r)
perstAPI _ = Proxy

-- class HasClient (PerstAPI t r) => HasPerstClient (t::TreeDef) (r :: Type) where
perstClient :: HasClient (PerstAPI t r) => Proxy '(t,r) -> Client (PerstAPI t r)
perstClient = client . perstAPI

getList :: Client (PerstAPI t r) -> Condition t r -> ClientM [r]
getList (c :<|> _) = c
getRec :: Client (PerstAPI t r) -> PK t r -> ClientM r
getRec (_ :<|> c :<|> _) = c
insRec :: Client (PerstAPI t r) -> r -> ClientM (PK t r)
insRec (_ :<|> _ :<|> c :<|> _) = c
updRec :: Client (PerstAPI t r) -> r -> ClientM NoContent
updRec (_ :<|> _ :<|> _ :<|> c :<|> _) = c
updDiffRec :: Client (PerstAPI t r) -> (r,r) -> ClientM NoContent
updDiffRec (_ :<|> _ :<|> _ :<|> _ :<|> c :<|> _) = c
delRec :: Client (PerstAPI t r) -> PK t r -> ClientM NoContent
delRec (_ :<|> _ :<|> _ :<|> _ :<|> _ :<|> c) = c

-- class ClientPerstAPI (t :: TreeDef) r where
--   getList     :: Condition t r -> ClientM [r]
--   getRec      :: PK t r -> ClientM r
--   insRec      :: r -> ClientM (PK t r)
--   updRec      :: r -> ClientM NoContent
--   updDiffRec  :: (r,r) -> ClientM NoContent
--   delRec      :: PK t r -> ClientM NoContent

-- instance ClientPerstAPI t r where
--   getList :<|> getRec :<|> insRec :<|> updRec :<|> updDiffRec :<|> delRec
--     = client (Proxy :: Proxy (PerstAPI t r))

-- serverPerstAPI :: (DMLTree b t r, SelTreeCond b t r, SelTreeCons b t (PK t r) r)
--     => Proxy# '(b,t,r) -> Conn b -> Server (PerstAPI t r)
-- serverPerstAPI (btr::Proxy# '(b,t,r)) conn =
--   enter (sessionToHandler (proxy# :: Proxy# b) conn) (serverPerstSessAPI btr)
