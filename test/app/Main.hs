{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}
{-# OPTIONS_GHC -Wno-missing-signatures -Wno-unused-imports #-}

module Main where

import           Data.Bifunctor     (first)
import           Data.Tagged        (Tagged (..))
import           GHC.Generics
import           Lens.Micro

import           Data.Type.GrecTree
import           Perst.Test.Data    (check)

data Pis = Pis Int String deriving (Eq, Show, Ord, Generic)

data X = XInt Int | XChar Char | XPis Pis | XNull deriving (Show, Eq)
instance Convert Int [X] where convert = (:[]) . XInt
instance Convert a [X] => Convert (Maybe a) [X] where
  convert = maybe [XNull] convert
instance Convert Char [X] where convert = (:[]) . XChar
instance Convert Pis [X] where convert = (:[]) . XPis
instance Convert [X] (Int,[X]) where convert (XInt x : xs) = (x,xs)
instance Convert [X] (a,[X]) => Convert [X] (Maybe a,[X]) where
  convert (XNull : xs) = (Nothing,xs)
  convert xs           = first Just $ convert xs
instance Convert [X] (Char,[X]) where convert (XChar x : xs) = (x,xs)
instance Convert [X] (Pis,[X]) where convert (XPis x : xs) = (x,xs)
instance SConvNames AllFld s Int
instance SConvNames AllFld s Char
instance SConvNames AllFld s Pis
-- instance SConvNames AllFld s (Maybe Int) where
--   type SFldTypes AllFld s (Maybe Int) = '[Maybe Int]

main :: IO ()
main = print check0 >> check


data DatBig = RecBig
    { fb1  :: Int
    , fb2  :: Char
    , fb3  :: Pis
    , fb4  :: Int
    , fb5  :: Char
    , fb6  :: Pis
    , fb7  :: Int
    , fb8  :: Char
    , fb9  :: Pis
    , fb10 :: Int
    , fb11 :: Char
    , fb12 :: Pis
    , fb13 :: Int
    , fb14 :: Char
    , fb15 :: Pis
    , fb16 :: Int
    , fb17 :: Char
    , fb18 :: Pis
    , fb19 :: Int
    , fb20 :: Int
    , fb21 :: Char
    , fb22 :: Pis
    , fb23 :: Int
    , fb24 :: Char
    , fb25 :: Pis
    , fb26 :: Int
    , fb27 :: Char
    , fb28 :: Pis
    , fb29 :: Int
    , f30::Int,f31::Int,f32::Int,f33::Int,f34::Int,f35::Int,f36::Int,f37::Int,f38::Int,f39::Int
    , f40::Int,f41::Int,f42::Int,f43::Int,f44::Int, f45::Int,f46::Int,f47::Int,f48::Int,f49::Int
    } deriving (Show, Generic, Eq)
instance Grec DatBig
instance ConvNames t DatBig
rdb = RecBig 5 'x' (Pis 2 "y") 1 'x' (Pis 2 "y") 1 'x' (Pis 2 "y")
       1 'x' (Pis 2 "y") 1 'x' (Pis 2 "y") 1 'x' (Pis 2 "y") 2
       1 'x' (Pis 2 "y") 1 'x' (Pis 2 "y") 1 'x' (Pis 2 "y") 2
        1 2 3 4 5 6 7 8 9 0
        1 2 3 4 5 6 7 8 9 0

trdb :: GrecTagged DatBig
trdb = toTagged rdb
rdb' = fromTagged $ trdb & tlens' @"fb1" .~ trdb ^. tlens' @"fb19" :: DatBig

data Dat1 = Rd1 { f11 :: Int } deriving (Show, Eq, Generic)
instance Grec Dat1
vr1 = Rd1 1

data Dat4 = Rec4
  { f41 :: Maybe Int
  , f42 :: Char
  , f43 :: Pis
  , f44 :: Int
  , f45 :: Int
  , f46 :: Char
  } deriving (Show, Eq, Generic)
instance Grec Dat4
instance ConvNames t Dat4
type instance GPlus Dat4 = True
vr4 = Rec4 (Just 1) 'x' (Pis 2 "test") 3 4 'z' :: Dat4
vr4' :: Dat4
vr4' = fromTagged $ toTagged vr4 & tlens' @"f41" .~ Nothing

type T = (Tagged "x" Dat4, Tagged () Dat4 -- , Tagged "mb" (Maybe Dat4)
         , Tagged "z" (Tagged "1_" Dat4, Tagged (Just "2_") Dat4))

r :: T
r = (Tagged vr4, Tagged vr4', {-Tagged (Just vr4), -}Tagged (Tagged vr4,Tagged vr4)) :: T

trdb' :: DeletedType '["f43","f41","y","fb12","fb15","x"] (GrecTagged DatBig)
trdb' = tdel @["f43","f41","y","fb12","fb15","x"] trdb

-- instance (Grec v, SConvNames (TaggedTag (GrecTagged v)) (Untag (GrecTagged v)))
--       => SConvNames ms1 (Tagged () v) where
--   type SFldNames ms1 (Tagged () v)
--     = SFldNames (TaggedTag (GrecTagged v)) (Untag (GrecTagged v))
--   type SFldTypes ms1 (Tagged () v)
--     = SFldTypes (TaggedTag (GrecTagged v)) (Untag (GrecTagged v))
--   getSFldNames = getSFldNames @(TaggedTag (GrecTagged v)) @(Untag (GrecTagged v))

type TT = (Int,Tagged "xx" Dat4,Char)
-- tt :: GrecTagged TT
tt = toTagged ((5,Tagged vr4,'z') :: TT)

check0 :: Bool
check0 =  rdb /= rdb'
      && rdb == rdb' {fb1 = fb1 rdb}
      && r == (fromTagged $ toTagged r)
      -- && toTagged (Tagged vr4::Tagged "_" Dat4, vr4)
      --       ^. tlens' @1 . tlens @"f42" == 'x'
      && tdel @["f43","f41","y","fb12","fb15","x"] trdb
        == tdel @["fb12","x","fb15","y","f43","f41","x"] trdb
      && convert tt
        == [XInt 5,XInt 1,XChar 'x',XPis (Pis 2 "test")
           ,XInt 3,XInt 4,XChar 'z',XChar 'z']
      && convert @[X] (convert tt) == (tt,[]::[X])
      && length (convert (toTagged r) :: [X]) == length (fldNames @AllFld @T)
