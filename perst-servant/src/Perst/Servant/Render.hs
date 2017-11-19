{-# LANGUAGE DefaultSignatures #-}
module Perst.Servant.Render where

import           Data.Int       (Int64)
import           Data.Tagged    (Tagged (..))
import qualified Data.Text      as T
import           Lucid

import           Data.Type.Grec (Convert (..))

-- class Render a where
--   renderTitle :: a -> Html ()
--   renderTitle _ = mempty
--   renderEdit  :: a -> Html ()
--   renderCell  :: a -> Html ()
--   default renderCell :: Show a => a -> Html ()
--   renderCell = toHtml . show
--
-- instance Render Int64 where
--   renderEdit a = input_ [type_ "number", value_ $ T.pack $ show a]
--
-- instance Render T.Text where
--   renderEdit a = input_ [type_ "text", value_ a]
--   renderCell = toHtml
--
-- instance Render Double where
--   renderEdit a = input_ [type_ "number", step_ "any", value_ $ T.pack $ show a]
--
data RenderType = RenderTitle | RenderEdit | RenderCell

type Html_ = Html ()

instance Convert (Tagged RenderEdit Int64) Html_ where
  convert (Tagged a) = input_ [type_ "number", value_ $ T.pack $ show a]

instance Convert (Tagged RenderCell Int64) Html_ where
  convert = toHtml . show . unTagged

instance Convert (Tagged RenderEdit Double) Html_ where
  convert (Tagged a)
    = input_ [type_ "number", step_ "any", value_ $ T.pack $ show a]

instance Convert (Tagged RenderCell Double) Html_ where
  convert = toHtml . show . unTagged

instance Convert (Tagged RenderEdit T.Text) Html_ where
  convert (Tagged a) = input_ [type_ "text", value_ a]

instance Convert (Tagged RenderCell T.Text) Html_ where
  convert = toHtml . unTagged

instance Convert (Tagged rt a) Html_
      => Convert (Tagged (rt::RenderType) (Tagged '[n] a)) Html_ where
  convert = convert . fmap unTagged

instance ( Convert (Tagged rt (Tagged '[n1] a)) Html_
         , Convert (Tagged rt (Tagged (n2 ':ns) a)) Html_
         )
      => Convert (Tagged (rt::RenderType) (Tagged (n1 ':n2 ':ns) a)) Html_ where
  convert = convert . fmap unTagged
