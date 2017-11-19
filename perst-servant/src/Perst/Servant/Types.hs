module Perst.Servant.Types where

import           Data.Aeson  (FromJSON (..), ToJSON (..))
import           Data.Tagged (Tagged (..), untag)
import           Servant     (FromHttpApiData (..), ToHttpApiData (..))


newtype PerstRes t r = PerstRes { getPerstRes :: r } deriving (Show, Eq)
instance FromJSON r => FromJSON (PerstRes t r) where
  parseJSON = fmap PerstRes . parseJSON
instance ToJSON r => ToJSON (PerstRes t r) where
  toJSON = toJSON . getPerstRes

-- to avoid orphan instances
newtype PerstPar r = PerstPar { getPerstPar :: r } deriving (Show, Eq)

instance FromHttpApiData v => FromHttpApiData (PerstPar (Tagged x v)) where
  parseUrlPiece = fmap (PerstPar . Tagged) . parseUrlPiece
instance ToHttpApiData v => ToHttpApiData (PerstPar (Tagged x v)) where
  toUrlPiece = toUrlPiece . untag . getPerstPar
