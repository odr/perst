{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE UndecidableInstances      #-}

module Main where

-- main :: IO ()
-- main = return ()

import           Control.Applicative
import           Data.Bifunctor
import           Data.Functor.Compose
import           Data.Int                      (Int64)
import qualified Data.Text                     as T

import           Control.Monad.Catch           (SomeException, catch)
import           Control.Monad.IO.Class        (MonadIO (..))
import           Data.List                     (sort)
import           Data.Proxy                    (Proxy (..))
import           Data.Singletons.CustomStar    (singletonStar)
import           Data.Singletons.Prelude
-- import           Data.Singletons.Prelude.List
import           Data.Singletons.Prelude.Maybe
import           Data.Tagged                   (Tagged (..))
import           GHC.Generics                  (Generic)

import           Data.Text                     hiding (map)
import           Data.Type.Grec
-- import           Perst.Database.Constraints    (InsConstr)
import           Perst.Database.DataDef        (DataAutoIns, DataDef' (..),
                                                DataDefInfo (..), DataInfo (..),
                                                DelCons (..), FK (..))
import           Perst.Database.DbOption       (DbOption (..), DbTypeName,
                                                DbTypeNames (..), SessionMonad)
import           Perst.Database.DDL            as DDL
import           Perst.Database.DML            (DML (..))
import           Perst.Database.DMLTree        (DMLTree (..))
import           Perst.Types

import           DB
import           Tabs
----------------------
----------------------





createTab :: DDL DB a => Proxy a -> SessionMonad DB IO ()
createTab (p :: Proxy a) = do
  catch (DDL.drop @DB @a) (\(_::SomeException) -> return ())
  create @DB @a

o1 = Order 0 "1" 1 Nothing

ct = [ CustomerTree 1 "odr" (Just "odr") -- "odr@ru"
        [ OrderTree 1 "1" "01.01.2017"
          [ OrderPosition 1 1 2 11.11
          , OrderPosition 1 2 5 12.22
          ]
        , OrderTree 2 "2" "01.02.2017" [ OrderPosition 2 1 1 5.0 ]
        , OrderTree 3 "3" "01.03.2017" []
        ]
        [ Address 1 1 "My street"
        , Address 2 1 "My second street"
        ]
    , CustomerTree 2 "dro" Nothing -- "dro@ru"
        [ OrderTree 4 "1" "01.04.2017" []
        ]
        [ Address 3 2 "Some street"]
    , CustomerTree 3 "zev" Nothing -- "zev@zev"
        [] []
    ]
-- pct = Proxy :: Proxy (FieldsTree r)

-- type ITC t r = InsertTreeConstraint IO ZipList DB t r
--
-- check :: ITC t r => Proxy '(t, r)
-- check = Proxy
--
-- type IC t r = InsConstr IO DB (TdData t) r
-- type ICH t r = InsertChilds IO ZipList DB (DdAutoIns (TdData t))
--               (DdKey (TdData t)) (GrecChilds t r) r
--
-- type ICH' t r xs = InsertChilds IO ZipList DB (DdAutoIns (TdData t))
--               (DdKey (TdData t)) xs r
--
-- check1 :: IC t r => Proxy '(t, r)
-- check1 = Proxy
--
-- check2 :: ICH t r => Proxy '(t, r)
-- check2 = Proxy
--
-- check2' :: ICH' t r xs => Proxy '(t, r, xs)
-- check2' = Proxy
--
-- checkGL :: GrecLens s [FieldByName s r] r => Proxy '(s,r)
-- checkGL = Proxy
--
-- checkGL2 :: NamesGrecLens (Fsts rs) (RecParent r rs) r => Proxy '(rs,r)
-- checkGL2 = Proxy

main :: IO ()
main = runSession @DB "test.db" $ do
  -- createTab pTab
  createTab (Proxy :: Proxy TCustomer)
  createTab (Proxy :: Proxy TOrder)
  createTab (Proxy :: Proxy TArticle)
  createTab (Proxy :: Proxy TOrderPosition)
  createTab (Proxy :: Proxy TAddress)

{-
  insertManyR pCustomer [ Customer 1 (GrecGroup $ Names "odr" (Just "odr")) "x"
                        , Customer 2 (GrecGroup $ Names "dro" Nothing) "y"
                        , Customer 3 (GrecGroup $ Names "zev" Nothing) "z"
                        ]
  rs <- insertManyR pOrder  [ Order 0 "1" 1 Nothing  "01.01.2017"
                            , Order 0 "2" 1 (Just 2) "01.02.2017"
                            , Order 0 "3" 1 (Just 1) "01.03.2017"
                            , Order 0 "1" 2 (Just 3) "01.04.2017"
                            ]
  insertManyR pArticle  [ Article 1 "art1" 12.22
                        , Article 2 "art2" 3.14
                        ]
  insertManyR pOrderPosition  [ OrderPosition 1 2 5 12.22
                              , OrderPosition 1 1 2 11.11
                              , OrderPosition 2 1 1 5
                              ]
  insertManyR pAddress  [ Address 1 1 "My street"
                        , Address 2 1 "My second street"
                        , Address 3 2 "Some street"
                        ]
-}

  insertTreeMany @DB @TCustomerTree ct

  ct' <- selectTreeMany @DB @TCustomerTree @CustomerTree
                (map Tagged [1..3] :: [Tagged '["id"] Int64])
        -- >>= liftIO . putStrLn . ("Check CustomerTree: " ++) . show
                   -- . (== map (:[]) ct) . sort
  liftIO $ putStrLn $ "Check CustomerTree: " ++
    if ct' == map (:[]) ct
      then "Checked"
      else "ct' = " ++ show ct'

{-
  let ords = [ Order (rs!!3) "z4" 3 (Just 1)
             , Order (rs!!1) "z2" 2 Nothing
             ]
  updateByPKManyR pOrder ords
  updateByKey pCustomer ( Tagged "dro"    :: Tagged '["name"]  T.Text
                        , Tagged "numnum" :: Tagged '["email"] T.Text
                        )
- }
  updateByKey pCustomer
              ( Tagged "odr"             :: Subrec TCustomer '["name"]
              -- , Tagged ("zu",(4,"odr1")) :: Subrec TCustomer '["email","id","name"]
              , Tagged ("zu",4,"odr1") :: Tagged '["email","id","name"] (T.Text,Int64,T.Text)
              )
 { -
  updateByPKR pCustomer $ Customer 2 "drodro" "z"

  deleteByPKR pOrder (Order 3 "3" 1 $ Just 1)
{-

-}
  selectManyR pCustomer
              (Proxy :: Proxy Customer)
              (map Tagged [2,3] :: [Tagged '["id"] Int64])
        >>= liftIO . print

-}
  return ()
