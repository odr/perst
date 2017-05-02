{-# LANGUAGE GADTs                #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}
module Perst.Database.Join
    (
    ) where
import           Data.Kind               (Type)
import           Data.List               (sortBy)
import           Data.Proxy              (Proxy (..))
import           Data.Singletons.Prelude
import           Data.Singletons.TH
import           Data.Tree
import           Data.Type.Grec
import           GHC.TypeLits            (Symbol)
import           Perst.Database.Types
import           Perst.Types



{-
promoteOnly
  [d|
    -- data Join t = Join
    --   { jType       :: t
    --   , jResDataDef :: [DataDef]
    --   , jRefs       :: [(DataDef,[Symbol])]

    --   }

    joinFldTypes :: (Eq s)
        => [(s,d)] -> t -> (d -> [(s,t)]) -> (t -> [(s,t)]) -> Maybe [[t]]
    joinFldTypes dds t f1 f2 = case mdds of
      Nothing   -> Nothing
      Just ddss -> res (map f1 ddss)
     where
      r1 = f2 t
      mdds = submap (map fst r1) dds
      r2 = map (map fst . f2 . snd) r1
      -- res :: [[(s,t)]] -> Maybe [[t]]
      res = foldr proc (Just []) . zipWith submap r2
       where
        proc Nothing _           = Nothing
        proc _ Nothing           = Nothing
        proc (Just vs) (Just xs) = Just $ vs : xs

    recTypess :: t -> (t -> [t]) -> [[t]]
    recTypess x f = map f $ f x
  |]

joinByKey ::
  (JoinFldTypes j r DdRecSym0 FieldsGrecSym0
      ~ Just (RecTypess r FieldTypesGrecSym0)
  )
  => Proxy (j :: [(Symbol, DataDef)]) -> Proxy (r :: Type) -> [r]
joinByKey = undefined

-- joinByKey2 ::
--   (JoinFldTypes j r DdRecSym0 FieldsGrecSym0
--       ~ Just (RecTypess r FieldTypesGrecSym0)
--   )
--   => Proxy (j :: [DataDef]) -> Proxy (r :: Type) -> [r]
-- joinByKey2 = undefined
--
















-- jtree :: (a -> [a]) -> a -> [a] -> [Tree a] -> [Tree  a]
-- jtree = undefined
--
-- jt :: Eq a => (a -> [a]) -> (a -> a -> Ordering)-> a -> [a] -> Tree a -> [Tree  a]
-- jt f comp x xs (Node z zs) = addParent ++ addChild (Node z zs)
-- -- when we take a reference we can't take it again!
-- -- it is not done yet
--  where
--   addParent
--     | z `elem` f x  = [Node x [Node z zs]]
--     | otherwise     = []
--
--   addChild (Node z zs) = addTop ++ iter [] zs
--    where
--     addTop
--       | x `elem` f z  = [Node z (Node x [] : zs)]
--       | otherwise     = []
--     iter as [] = []
--     iter as (b:bs)
--       = map (\t -> Node z
--                     $ sortBy (\t1 t2 -> comp (rootLabel t1) (rootLabel t2))
--                     -- there could be equivalent forests. We should take one...
--                     $ (t : as) ++ bs)
--             $ addChild b
--       ++ iter (b:as) bs
--
--
--
-- -- jtree :: (a -> [a]) -> [a] -> Maybe (Tree  a)
-- -- jtree _ []      = Nothing
-- -- jtree f (x:xs)  = go x xs []
-- --  where
-- --   go x xs zs = concatMap jt zs
-- --    where
-- --     jt (Node z zs) =
--
--
-- -- mbOne $ foldr (jcases f) [] xs
-- --  where
-- --   mbOne [x] = Just x
-- --   mbOne _   = Nothing
-- --
-- -- jcases :: (a -> [a]) -> a -> [[Tree a]] -> [[Tree a]]
-- -- jcases f x zs =
--
-- -- jtree :: (a -> b) -> (b -> b -> Bool) -> [a] -> Maybe (Tree a)
-- -- jtree f1 f2 xs
-- --   = mbOne $ concat $ filter isOne $ foldr (jcases f1 f2) [] xs
-- --  where
-- --   isOne [_] = True
-- --   isOne _   = False
-- --   mbOne [x] = Just x
-- --   mbOne _   = Nothing
-- --
-- -- jcases :: (a -> b) -> (b -> b -> Bool) -> a -> [[Tree a]] -> [[Tree a]]
-- -- jcases f1 f2 a [] = [[Node a []]]
-- -- jcases f1 f2 a cs = concatMap (jfrst f1 f2 a) cs
-- --
-- -- jfrst :: (a -> b) -> (b -> b -> Bool) -> a -> [Tree a] -> [[Tree a]]
-- -- jfrst f1 f2 a ts = undefined -- concatMap (jt f1 f2 a) ts
-- --
-- -- jt :: (a -> b) -> (b -> b -> Bool) -> a -> Tree a -> [Tree a]
-- -- jt f1 f2 a1 (Node a2 as)
-- --   = map snd (filter fst
-- --       [ (f2 (f1 a1) (f1 a2), Node a1 [Node a2 as])
-- --       , (f2 (f1 a2) (f1 a1), Node a2 (Node a1 [] : as))
-- --       ])
-- --     ++ concat (iter (jt f1 f2 a1) (Node a2) [] as)
-- --  where
-- --   iter :: (a -> [a]) -> ([a] -> a) -> [a] -> [a] -> [[a]]
-- --   iter f g as1 (a : as2)
-- --     | null as = rest
-- --     | otherwise = map (\v -> g $ v : as1 ++ as2) as : rest
-- --    where
-- --     as = f a
-- --     rest = iter f g (a : as1) as2
-- --   iter _ _ as1 [] = []
-- --
-}
