module Perst.Database.DDL
    ( DDL(..)
    , rowCreate
    ) where

import           Control.Monad.IO.Class (MonadIO)
import           Data.Kind              (Type)
import           Data.Proxy             (Proxy (..))
import           Data.Type.Grec
import           Perst.Database.Types
import           Perst.Types

class DDL b (t :: DataDef) where
    ddlCreate :: (MonadIO m) => Proxy t -> SessionMonad b m ()
    ddlDrop   :: (MonadIO m) => Proxy t -> SessionMonad b m ()

rowCreate :: TabConstrB b t
          => Proxy (b :: Type) -> Proxy (t :: DataDef) -> [String]
rowCreate  (pb :: Proxy b) (pt :: Proxy t)
        = zipWith (\n t -> n ++ " " ++ t) ns ts
  where
    ns  = fieldNames pt
    ts  = dbTypeNames pb pt
