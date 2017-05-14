{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# OPTIONS_GHC -fprint-explicit-kinds #-}

module Perst.Database.TreeDef
    -- (
    -- )
    where

import           Control.Applicative        (ZipList (..))
import           Control.Arrow              ((&&&))
import           Control.Monad              (liftM2)
import           Control.Monad.Catch        (MonadMask)
import           Control.Monad.IO.Class     (MonadIO (..))
import           Data.Bifunctor             (second)
import           Data.Kind                  (Type)
import           Data.List                  (foldl')
import           Data.Maybe                 (catMaybes, isJust)
import           Data.Singletons.Prelude
import           Data.Singletons.TH
import qualified Data.Text.Lazy             as TL

import           Data.Type.Grec             (ConvFromGrec (..), ConvToGrec (..),
                                             ConvTree (..), Convert (..),
                                             FieldsTree, TreeChilds, TreeRec,
                                             TreeT, TreeT' (..))

import           Perst.Database.Constraints
import           Perst.Database.DataDef
import           Perst.Database.DbOption
import           Perst.Database.DML

singletons [d|
  data TreeDef' s t = TreeDefC (DataDef' s t) [(TreeDef' s t, [(s,s)])]

  tdData    (TreeDefC d _) = d
  tdChilds  (TreeDefC _ c) = c

  parentKeyNames t = map (map fst . snd) $ tdChilds t
  |]

type TreeDef = TreeDef' Symbol Type

{-
data TreeT' s t = TreeT { treeRec     :: [(s,t)]
                        , treeChilds  :: [(s, TreeT' s t)]
                        }
type TreeT = TreeT' Symbol Type
type FieldsTree a = GFieldsTree (Rep a) -- :: TreeT

data ConvTree a = ConvTree [a] [[ConvTree a]] deriving (Eq, Show)

-}
-- selectMany' :: (MonadIO m, MonadMask m, DelConstr b t k)
--             => Proxy t -> [TL.Text] -> [k] -> SessionMonad b m [[[FieldDB b]]]

-- class ProcessProxy a b c where
--   processProxy :: Proxy a -> (Proxy b -> c) -> c
--
-- instance (Monoid c, ProcessProxy as) => ProcessProxy (a ': as) a  c where
--   processProxy _ f = f (Proxy :: Proxy a) <> processProxy (Proxy :: Proxy as) f

selectTreeMany
  :: (MonadIO m, MonadMask m
     , Convert (ConvTree (FieldDB b)) r
     , SingI (Map FstSym0 (TreeRec (FieldsTree r)))
     , DelConstr b (TdData t) k
     , ProcessChilds b (TdChilds t) (TreeChilds (FieldsTree r))
     , SingI (ParentKeyNames t)
     , Show (FieldDB b)
     )
  => Proxy (t :: TreeDef) -> Proxy (r :: Type) -> [k] -> SessionMonad b m [[r]]
selectTreeMany (pt :: Proxy t) (pr :: Proxy r) (ks :: [k])
  = map (map convert) <$> selectTreeMany' pt
                                (Proxy :: Proxy (FieldsTree r))
                                (fieldNamesT (Proxy :: Proxy k))
                                (map convFromGrec ks)

selectTreeMany' :: (MonadIO m, MonadMask m , DbOptionConstr b (TdData t)
                   , SingI (Map FstSym0 (TreeRec r))
                   , SingI (ParentKeyNames t)
                   , ProcessChilds b (TdChilds t) (TreeChilds r)
                   , Show (FieldDB b)
                   )
                => Proxy (t :: TreeDef) -> Proxy (r :: TreeT)
                -> [TL.Text] -> [[FieldDB b]]
                -> SessionMonad b m [[ConvTree (FieldDB b)]]
selectTreeMany' (pt :: Proxy t) (pr :: Proxy r) keyNames keys
  = -- do
    -- liftIO $ putStrLn "========== selectTreeMany' ==========="
    -- liftIO $ putStrLn $ "keyNames: " ++ show keyNames
    -- liftIO $ putStrLn $ "keys: " ++ show keys
    -- (r, parentKeys) <- unzip . map (unzip . map recAndKeys)
    --                 <$> selectMany' ptd fldNames keyNames keys

    selectMany' ptd fldNames keyNames keys
      >>= mapM (mapM $ fmap (uncurry ConvTree)
                     . sequence
                     . second (processChilds ptc prc)
                     . recAndKeys)

    -- liftIO $ putStrLn $ "r: " ++ show r
    -- liftIO $ putStrLn $ "parentKeys: " ++ show parentKeys
    -- chs <- processChilds ptc prc parentKeys
    -- liftIO $ putStrLn $ "chs: " ++ show chs
    -- let res = zipWith ({- by result -} zipWith ConvTree) r chs
    -- liftIO $ putStrLn $ "res: " ++ show res
    -- liftIO $ putStrLn "========== end ==========="
    -- return res
 where
  recNames = map TL.pack $ showProxy (Proxy :: Proxy (Map FstSym0 (TreeRec r)))
  parentKeyNames  = map (map TL.pack)
                  $ showProxy (Proxy :: Proxy (ParentKeyNames t))
  fldNames = recNames `mappend` mconcat parentKeyNames
  recLen = length recNames
  parentKeyLens = map length parentKeyNames
  recAndKeys rs = (r1, splitByCnt rs1 parentKeyLens)
   where
    (r1,rs1) = splitAt recLen rs
  prc = Proxy :: Proxy (TreeChilds r)
  ptc = Proxy :: Proxy (TdChilds t)
  ptd = Proxy :: Proxy (TdData t)

splitByCnt :: [a] -> [Int] -> [[a]]
splitByCnt xs = reverse . fst
    . foldl'(\(res, rest) l ->
        let (rn, rsn) = splitAt l rest in (rn : res, rsn)
      ) ([], xs)

class ProcessChilds b (t :: [(TreeDef, [(Symbol,Symbol)])])
                      (r :: [(Symbol, TreeT)]) where
  processChilds :: (MonadIO m, MonadMask m)
                => Proxy t -> Proxy r -> [[FieldDB b]]
                -> SessionMonad b m [[ConvTree (FieldDB b)]]

instance ProcessChilds b '[] '[] where
  processChilds _ _ _ = return []

instance (ProcessChilds b ts rs, DbOptionConstr b (TdData t1)
    , SingI (ParentKeyNames t1), SingI (Map FstSym0 (TreeRec r2))
    , ProcessChilds b (TdChilds t1) (TreeChilds r2)
    , SingI (Map SndSym0 t2)
    , Show (FieldDB b)
    )
    => ProcessChilds b ('(t1,t2) ': ts) ('(r1,r2) ': rs)
 where
  processChilds _ _ ks
    = liftM2 (:)
        (concat <$> selectTreeMany' (Proxy :: Proxy t1)
                        (Proxy :: Proxy r2) keyNames (take 1 ks))
        (processChilds (Proxy :: Proxy ts) (Proxy :: Proxy rs) (tail ks))
   where
    keyNames = map TL.pack $ showProxy (Proxy :: Proxy (Map SndSym0 t2))


-- class ProcessChilds b (t :: [(TreeDef, [(Symbol,Symbol)])])
--                       (r :: [(Symbol, TreeT)]) where
--   processChilds :: (MonadIO m, MonadMask m)
--                 => Proxy t -> Proxy r -> [[[[FieldDB b]]]]
--                 -> SessionMonad b m [[[[ConvTree (FieldDB b)]]]]
--
-- instance ProcessChilds b '[] '[] where
--   processChilds _ _ _ = return []
--
-- instance (ProcessChilds b ts rs, DbOptionConstr b (TdData t1)
--     , SingI (ParentKeyNames t1), SingI (Map FstSym0 (TreeRec r2))
--     , ProcessChilds b (TdChilds t1) (TreeChilds r2)
--     , SingI (Map SndSym0 t2)
--     , Show (FieldDB b)
--     )
--     => ProcessChilds b ('(t1,t2) ': ts) ('(r1,r2) ': rs)
--  where
--   processChilds _ _ ks = do
--     selectTreeMany'
--     -- liftIO $ putStrLn "processChilds"
--     -- liftIO $ putStrLn $ "keys: " ++ show ks
--     -- liftIO $ putStrLn $ "keyNames: " ++ show keyNames
--     -- liftIO $ putStrLn $ "keysLen: " ++ show keysLen
--     -- liftIO $ putStrLn $ "ksh: " ++ show ksh
--     -- liftIO $ putStrLn $ "kst: " ++ show kst
--     child <- flip splitByCnt keysLen
--           <$> selectTreeMany' (Proxy :: Proxy t1) (Proxy :: Proxy r2)
--                               keyNames (concat ksh)
--     rest <- processChilds (Proxy :: Proxy ts) (Proxy :: Proxy rs) kst
--     -- liftIO $ putStrLn $ "child: " ++ show child
--     -- liftIO $ putStrLn $ "rest: " ++ show rest
--     return $ zipWith (zipWith (:)) child rest
--    where
--     keyNames = map TL.pack $ showProxy (Proxy :: Proxy (Map SndSym0 t2))
--     (ksh,kst) = unzip -- $ filter (\x -> case x of { ([],[]) -> False; _ -> True })
--               $ map (unzip . map (head &&& tail) {- . filter (not . null) -}) ks
--     keysLen = map length ksh
