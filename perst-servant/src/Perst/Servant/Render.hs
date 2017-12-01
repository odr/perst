{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeApplications  #-}
-- {-# LANGUAGE UndecidableInstances #-}
module Perst.Servant.Render where

import           Data.Int            (Int64)
import           Data.Maybe          (fromMaybe)
import           Data.Tagged         (Tagged (..), retag)
import qualified Data.Text           as T
import           GHC.TypeLits        (Symbol)
import           Lucid

import           Data.Type.Grec      (Convert (..), Grec (..))

import           Perst.Servant.Types

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

instance Convert (Tagged RenderEdit (Maybe Int64)) Html_ where
  convert (Tagged a)
    -- = input_ [type_ "number", value_ $ T.pack $ maybe "" show a]
    = input_ $ maybe [] ((:[]) . value_ . T.pack . show) a ++ [type_ "number"]

instance Convert (Tagged RenderCell (Maybe Int64)) Html_ where
  convert = toHtml . maybe "" show . unTagged

instance Convert (Tagged RenderEdit (Maybe Double)) Html_ where
  convert (Tagged a)
    -- = input_ [type_ "number", step_ "any", value_ $ T.pack $ maybe "" show a]
    = input_ $ maybe [] ((:[]) . value_ . T.pack . show) a
            ++ [type_ "number", step_ "any"]

instance Convert (Tagged RenderCell (Maybe Double)) Html_ where
  convert = toHtml . maybe "" show . unTagged

instance Convert (Tagged RenderEdit (Maybe T.Text)) Html_ where
  convert (Tagged a)
    = input_ $ maybe [] ((:[]) . value_ . T.pack . show) a ++ [type_ "text"]

instance Convert (Tagged RenderCell (Maybe T.Text)) Html_ where
  convert = toHtml . fromMaybe "" . unTagged

instance Convert (Tagged rt (Grec r)) Html_
      => Convert (Tagged (rt::RenderType) (PerstRes t r)) Html_ where
  convert = convert . fmap (Grec . getPerstRes)


-- instance Convert (Tagged rt a) Html_
--       => Convert (Tagged ('(rt,'[n]) :: (RenderType,[Symbol])) a) Html_ where
--   convert = convert . (retag :: Tagged '(rt,'[n]) a -> Tagged rt a)
--
-- instance ( Convert (Tagged '(rt,'[n1]) a) Html_
--          , Convert (Tagged '(rt,n2 ':ns) as) Html_
--          )
--   => Convert (Tagged ('(rt, n1 ':n2 ':ns) :: (RenderType,[Symbol])) (a,as))
--              Html_ where
--   convert (Tagged (a,as)) = convert @_ @Html_ (Tagged @'(rt,'[n1]) a)
--                          >> convert (Tagged @'(rt,n2 ':ns) as)
--
-- instance ( Convert (GWOTagged '[] (Grec t)) Html_
--          , NamesGrecLens (FieldNamesGrec (Grec t)) (GWOPairs ns a) a
--   Convert (Tagged (rt::RenderType) (PerstRes t)) Html_
