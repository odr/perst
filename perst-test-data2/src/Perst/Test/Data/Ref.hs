{-# LANGUAGE DataKinds #-}
module Perst.Test.Data.Ref where

--
import           Perst.Database.DataDef (DelCons (..), Ref' (..))

type Refs =
  '[RefC "addCust"   "address" "customer" '[ '("customerId", "id")]   DcCascade
  , RefC "ordCust"   "orders"  "customer" '[ '("customerId", "id")]   DcCascade
  , RefC "ordCoCust" "orders"  "customer" '[ '("coCustomerId", "id")] DcSetNull
  , RefC "opOrd" "OrderPosition"  "orders" '[ '("orderId", "id")] DcCascade
  , RefC "opArt" "OrderPosition"  "article" '[ '("articleId", "id")] DcRestrict

  ]
