{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module CustomerTree where

import           Data.Int               (Int64)
-- import           Data.Singletons.Prelude (Proxy (..), Sing, SingI (sing))
import           Data.Tagged            (Tagged)
import qualified Data.Text              as T
import           GHC.Generics           (Generic)
import           GHC.TypeLits           (Symbol)

-- import           Data.Type.Grec          (ConvFromGrec, Convert, Grec)
-- import           Perst.Database.Tree.Def (MkTreeDef, TreeDef0 (TreeDefC0))
-- import           Perst.Database.DbOption (DbOption (..), SessionMonad)
-- import           Perst.Database.DML      (MonadCons, UpdTextCons)
import           Data.Type.Grec         (Grec, GrecWith, GrecWithout)
import           Perst.Database.DML
import           Perst.Database.Sqlite  (Sqlite)
import           Perst.Database.TreeDef

import           Customer
import           Order

data CustomerTree = CustomerTree
  { id        :: Int64
  , name      :: T.Text
  , shortname :: Maybe T.Text
  , orders    :: [OrderTree]
  , address   :: [Address]
  } deriving (Show, Generic, Eq, Ord)

data OrderTree = OrderTree
  { id        :: Int64
  , num       :: T.Text
  , date      :: T.Text
  , positions :: [OrderPosition]
  } deriving (Show, Generic, Eq, Ord)

type TCustomerTree = TreeDefC TCustomer
  '[ '( "orders",  '(TOrderTree, '[ '("customerId","id") ]))
   , '( "address", '(TreeDefC TAddress '[], '[ '("customerId","id") ]))
   ]
type TOrderTree = TreeDefC TOrder
  '[ '("positions", '(TreeDefC TOrderPosition '[],  '[ '("orderId", "id") ]))]
--

instance SelectByKey Sqlite TCustomer (Tagged '["id"] Int64, Grec CustomerTree)
                                  (Tagged '["id"] Int64)
instance SelectByKey Sqlite TAddress (Tagged ('[] :: [Symbol]) (), Grec Address)
                      (GrecWith '["customerId"] (Tagged '["customerId"] Int64))

instance SelectByKey Sqlite TOrder (Tagged '["id"] Int64, Grec OrderTree)
                     (GrecWith '["customerId"] (Tagged '["customerId"] Int64))

instance SelectByKey Sqlite TOrderPosition (Tagged ('[] :: [Symbol]) (), Grec OrderPosition)
                      (GrecWith '["orderId"] (Tagged '["orderId"] Int64))



instance InsertTree Sqlite TCustomerTree (Grec CustomerTree)

-- instance Insert Sqlite TCustomer (Grec CustomerTree)

-- instance InsertTree Sqlite TOrderTree TCustomerOrderTree

type TCustomerOrderTree = ( Tagged '["customerId"] Int64
                          , GrecWithout '["customerId"] (Grec OrderTree)
                          )
-- instance Insert Sqlite TOrder TCustomerOrderTree

-- instance InsertTree Sqlite ('TreeDefC TAddress '[]) TCustomerAddressTree

type TCustomerAddressTree = ( Tagged '["customerId"] Int64
                            , GrecWithout '["customerId"] (Grec Address)
                            )

-- instance InsertTree Sqlite ('TreeDefC TOrderPosition '[]) TOrderOrderPosTree

type TOrderOrderPosTree = ( Tagged '["orderId"] Int64
                          , GrecWithout '["orderId"] (Grec OrderPosition)
                          )

-- instance Insert Sqlite TAddress TCustomerAddressTree

-- instance Insert Sqlite TOrderPosition TOrderOrderPosTree

instance SelectTree Sqlite TCustomerTree CustomerTree (Tagged '["id"] Int64)

instance SelectTree Sqlite TOrderTree OrderTree
                      (GrecWith '["customerId"] (Tagged '["customerId"] Int64))

instance UpdateTree Sqlite TCustomerTree (Grec CustomerTree)

instance DeleteTree Sqlite TCustomerTree (Grec CustomerTree)

instance SelectTree Sqlite ('TreeDefC TAddress '[]) Address
                      (GrecWith '["customerId"] (Tagged '["customerId"] Int64))

instance SelectTree Sqlite ('TreeDefC TOrderPosition '[]) OrderPosition
                      (GrecWith '["orderId"] (Tagged '["orderId"] Int64))

-- instance UpdateByKeyDiff Sqlite TCustomer (Grec CustomerTree)

-- instance DeleteByKey Sqlite TCustomer (GrecWith '["id"] (Grec CustomerTree))

instance UpdateTree Sqlite TOrderTree TCustomerOrderTree

instance DeleteTree Sqlite TOrderTree (Grec OrderTree)

-- instance UpdateByKeyDiff Sqlite TCustomer
--                          (GrecWithout '["id"] (Grec CustomerTree))
--                          (GrecWith '["id"] (Grec CustomerTree))

instance UpdateTree Sqlite ('TreeDefC TAddress '[]) TCustomerAddressTree

instance DeleteTree Sqlite ('TreeDefC TAddress '[]) (Grec Address)

instance DeleteTree Sqlite TOrderTree TCustomerOrderTree

-- instance DeleteByKey Sqlite TOrder (GrecWith '["id"] (Grec OrderTree))

instance DeleteTree Sqlite ('TreeDefC TAddress '[]) TCustomerAddressTree

-- instance UpdateByPKDiff Sqlite TOrder TCustomerOrderTree

instance DeleteTree Sqlite ('TreeDefC TOrderPosition '[]) (Grec OrderPosition)

-- instance UpdateByPKDiff Sqlite TAddress TCustomerAddressTree

-- instance DeleteByKey Sqlite TAddress (GrecWith '["id"] (Grec Address))

-- instance DeleteByKey Sqlite TOrder (GrecWith '["id"] TCustomerOrderTree)

-- instance DeleteByKey Sqlite TAddress (GrecWith '["id"] TCustomerAddressTree)

instance UpdateTree Sqlite ('TreeDefC TOrderPosition '[]) TOrderOrderPosTree

-- instance UpdateByKeyDiff Sqlite TOrder (GrecWithout '["id"] TCustomerOrderTree)
--                                        (GrecWith '["id"] TCustomerOrderTree)

-- instance DeleteByKey Sqlite TOrderPosition
--                       (GrecWith '["orderId", "articleId"] (Grec OrderPosition))

-- instance UpdateByKeyDiff Sqlite TAddress
--                         (GrecWithout '["id"] TCustomerAddressTree)
--                         (GrecWith '["id"] TCustomerAddressTree)

instance DeleteTree Sqlite ('TreeDefC TOrderPosition '[]) TOrderOrderPosTree

-- instance UpdateByPKDiff Sqlite TOrderPosition TOrderOrderPosTree

-- instance DeleteByKey Sqlite TOrderPosition
--                       (GrecWith '["orderId", "articleId"] TOrderOrderPosTree)

-- instance UpdateByKeyDiff Sqlite TOrderPosition
--                      (GrecWithout '["orderId", "articleId"] TOrderOrderPosTree)
--                      (GrecWith '["orderId", "articleId"] TOrderOrderPosTree)


-- type TCustomerTree = MkTreeDef
--   ( TreeDefC0 TCustomer
--     '[ '( "orders"
--         , '( TreeDefC0 TOrder
--             '[ '("positions"
--                 , '( TreeDefC0 TOrderPosition '[]
--                   , '[ '("orderId", "id") ]
--                   )
--                 )
--             ]
--           , '[ '("customerId","id") ]
--           )
--         )
--      , '( "address"
--         , '( TreeDefC0 TAddress '[]
--           , '[ '("customerId","id") ]
--           )
--         )
--      ]
--   )
--
-- sCustomerTree = sing :: Sing TCustomerTree
--
-- selCustTree :: (MonadCons m, DbOption b
--       , UpdTextCons b (TaggedAllParentKeys TCustomerTree, GrecF CustomerTree) k
--       , ConvFromGrec k [FieldDB b]
--       , Convert (FieldDB b) T.Text
--       , Convert (FieldDB b) (Maybe T.Text)
--       , Convert (Maybe T.Text) (FieldDB b)
--       , Convert (FieldDB b) Int64
--       , Convert Int64 (FieldDB b)
--       , Convert (FieldDB b) Double
--       , Convert Double (FieldDB b)
--       ) => [k] -> SessionMonad b m [[CustomerTree]]
-- selCustTree = selectTreeMany sCustomerTree Proxy
-- -- updCustTree = updateTreeManyR sCustomerTree
-- -- insCustTree = insertTreeManyR sCustomerTree
