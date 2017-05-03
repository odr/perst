module Data.Type.Grec.ConvTree(ConvTree(..), showConvTree) where

import           Data.Monoid    (Monoid (..))
import           Data.Semigroup (Semigroup (..))

data ConvTree a = ConvTree [a] [[ConvTree a]] deriving (Eq, Show)

instance Semigroup (ConvTree a) where
  ConvTree xs1 ts1 <> ConvTree xs2 ts2 = ConvTree (xs1 ++ xs2) (ts1 ++ ts2)

instance Monoid (ConvTree a) where
  mempty = ConvTree [] []
  mappend = (<>)

showConvTree ct = go 0 ct
 where
   go pad (ConvTree xs ts) = do
    --  putStrLn $ replicate pad ' ' <> replicate 20 '-'
     putStrLn $ replicate pad ' ' <> show xs
     mapM_ (\t -> do
        --  putStrLn ""
         mapM_ (go $ pad + 2) t
         putStrLn ""
       ) ts
