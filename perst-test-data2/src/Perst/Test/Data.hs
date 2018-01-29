{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# OPTIONS_GHC -Wno-missing-signatures -Wno-unused-imports #-}

module Perst.Test.Data where

import           Control.Applicative          (ZipList (..))
import           Control.Monad.IO.Class       (MonadIO (..))
import           Data.Int                     (Int64)
import           Data.Tagged                  (Tagged (..))
import qualified Data.Text                    as T
import           GHC.Prim                     (Proxy#, proxy#)

import           Data.Type.GrecTree
import           Perst.Database.Condition2
import           Perst.Database.DataDef
import           Perst.Database.DbOption      (DbOption (..), DbTypeName,
                                               SessionMonad)
import           Perst.Database.DDL           as DDL
import           Perst.Database.DML.Insert
import           Perst.Database.DML.Select
import           Perst.Database.Sqlite
import           Perst.Database.Tree.Delete
import           Perst.Database.Tree.Insert
import           Perst.Database.Tree.Select
import           Perst.Database.Tree.Update


import           Perst.Types
-- import           Perst.Database.DMLTree

import           Perst.Test.Data.Article
import           Perst.Test.Data.Customer
import           Perst.Test.Data.CustomerTree
import           Perst.Test.Data.Db           (Db)
import           Perst.Test.Data.Order
import           Perst.Test.Data.OrderTree
import           Perst.Test.Data.Ref


ct = [ CustomerTree 1 (Tagged $ Names "odr" $ Just "odr")
        (PChilds
        [ OrderTree 1 "1" "01.01.2017"
          (PChilds
          [ OrderPosition 1 1 2 11.11
          , OrderPosition 1 2 5 12.22
          ])
        , OrderTree 2 "2" "01.02.2017" (PChilds [ OrderPosition 2 1 1 5.0 ])
        , OrderTree 3 "3" "01.03.2017" (PChilds [])
        ])
        (PChilds
        [ Address 1 1 "My street" "11B"
        , Address 2 1 "My second street" "10"
        ]
        )
    , CustomerTree 2 (Tagged $ Names "dro" Nothing)
        (PChilds [ OrderTree 4 "1" "01.04.2017" (PChilds []) ])
        (PChilds [ Address 3 2 "Some street" "12C"])
    , CustomerTree 3 (Tagged $ Names "zev" Nothing)
                  (PChilds []) (PChilds[])
    ]
ct3 = CustomerTree 3 (Tagged $ Names "zev1" $ Just "zev")
                  (PChilds [OrderTree 5 "2" "01.12.2017" (PChilds [ OrderPosition 8 1 1 5.0 ])])
                  (PChilds[])

type Sch = SchemaC '[TCustomer, TOrder, TArticle, TOrderPosition, TAddress] Refs

instance DDL Db TCustomer Refs
instance DDL Db TOrder Refs
instance DDL Db TArticle Refs
instance DDL Db TOrderPosition Refs
instance DDL Db TAddress Refs

dropCreateTest = do
  dropCreate @Db @TCustomer @Refs
  dropCreate @Db @TOrder @Refs
  dropCreate @Db @TArticle @Refs
  dropCreate @Db @TOrderPosition @Refs
  dropCreate @Db @TAddress @Refs

type TCT = GrecTagged CustomerTree
initTest = do
  dropCreateTest
  -- insertManyDef (proxy# :: Proxy# '(Db,TCustomer)) $ map toTagged ct
  insertTreeManyDef (proxy# :: Proxy# '(Db,Sch,"customer")) $ ZipList $ map toTagged ct

check :: IO ()
check = runSession @Db "test.db" $ do
  _ <- initTest

  -- ct' <- selectManyDef (proxy# :: Proxy# '(Db,TCustomer, GrecTagged Customer))
  --       (ZipList (map Tagged [1..3] :: [Tagged (BTreeFromList '["id"]) Int64]))
  ct' <- (map (map fromTagged) . getZipList)
      <$> selectTreeManyDef
        (proxy# :: Proxy# '(Db,Sch,"customer", TCT))
        (ZipList (map Tagged [1..3] :: [Tagged (BTreeFromList '["id"]) Int64]))
  liftIO $ do
    putStrLn ""
    putStrLn ""
    putStrLn $ "Check CustomerTree: " ++
      if ct' == map (:[]) ct
        then "Checked"
        else "Not Checked!!! ct' = " ++ show ct'
    putStrLn ""
  --
  deleteTreeManyDef (proxy# :: Proxy# '(Db,Sch,"customer"))
    $ map toTagged $ take 1 ct
  updateTreeManyDef (proxy# :: Proxy# '(Db,Sch,"customer"))
    (map toTagged $ Prelude.drop 2 ct)
    [toTagged ct3]

  (ct' :: [[CustomerTree]]) <- (map (map fromTagged) . getZipList)
      <$> selectTreeManyDef
        (proxy# :: Proxy# '(Db,Sch,"customer", TCT))
        (ZipList (map Tagged [1..3] :: [Tagged (BTreeFromList '["id"]) Int64]))
  liftIO $ do
    putStrLn "After update:"
    print ct'
  (ct' :: [CustomerTree]) <- map fromTagged
      <$> selectTreeCond2Def
            (proxy# :: Proxy# '("customer",TCT))
            (Tree @'(Db,Sch) (pnot (pcmp @"id" ==? (1::Int64))) Empty)
  liftIO $ do
    putStrLn "After update with cond2:"
    print ct'
  --
  return ()

-- checkCond
--   = runConvCond (convCond @DB @(Condition TCustomerTree (Grec CustomerTree))
--     (Rec (Tagged def
--       & grecLens @"name" .~ [CvEq ("xx"::T.Text),CvEq "yy"]
--       & grecLens @"email" .~ [CvLike ("mail"::T.Text),CvEq "yy"]
--       & grecLens @"id" .~ [CvEq (1::Int64)]
--       & grecLens @"shortname" .~ [CvNull :: CondVal (Maybe T.Text)]
--       & grecLens @"orders" .~ [CsExists $ Rec $ Tagged  def
--           & grecLens @"num" .~
--               [ CvNot $ CvEq ("5" :: T.Text)]
--                   :: CondSub (Child "orders" TCustomerTree) (Grec OrderTree)
--               ]
--       & grecLens @"address" .~
--         [CsNotExists
--           $ Rec $ Tagged def -- & grecLens @"street" .~ [CvNot $ CvEq ("zzz" :: T.Text)]
--             :: CondSub (Child "address" TCustomerTree) (Grec Address)
--         ]
--     )))
--
-- -- f = SS.evalState (convCond @DB @(Condition Maybe (Grec CustomerTree)) (Rec (Tagged def & grecLens @"name" .~ Just (CvEq ("xx"::T.Text)) & grecLens @"id" .~ Just (CvEq (1::Int64)) & grecLens @"shortname" .~ Just (CvNull :: CondVal (Maybe T.Text)) ))) 0
