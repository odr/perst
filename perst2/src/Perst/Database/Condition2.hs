{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}
module Perst.Database.Condition2 where

import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.Reader       (ReaderT, ask, local,
                                                   runReaderT)
import           Control.Monad.Trans.State.Strict (State, evalState, get,
                                                   modify)
import           Data.Aeson                       hiding (Null)
import           Data.Aeson.Types                 (Parser, explicitParseField,
                                                   listParser, typeMismatch)
import           Data.Bifunctor                   (first, second)
import           Data.Kind                        (Type)
import qualified Data.Map                         as M
import           Data.Maybe                       (isNothing)
import           Data.Proxy                       (Proxy (..))
import           Data.Singletons.Prelude          (Fst, SingI (..), Snd,
                                                   SomeSing (..), fromSing)
import           Data.Tagged                      (Tagged (..))
import           Data.Text                        (Text, pack, unpack)
import qualified Data.Text                        as T
import           Data.Text.Format                 (Only (..))
import           GHC.Generics                     (Generic (..))
import           GHC.TypeLits                     (KnownSymbol, SomeSymbol (..),
                                                   Symbol, someSymbolVal,
                                                   symbolVal)

import           Data.Type.GrecTree
import           Perst.Database.DataDef
import           Perst.Database.DbOption

-- type CondCons a bk = (Show a, ToJSON a, FromJSON a, Convert a [bk]) -- (() :: Constraint) --

data CondType = TRec | TChild | TTree deriving (Show, Eq)
data CondTreeType = TTMain | TTChild deriving (Show, Eq)

data Cmp = (:==) | (:<=) | (:>=) | (:>) | (:<) | Like deriving (Show, Eq, Generic)
instance FromJSON Cmp
instance ToJSON Cmp
data BoolOp = And | Or deriving (Show, Eq, Generic)
instance FromJSON BoolOp
instance ToJSON BoolOp

showCmp :: Cmp -> Text
showCmp = \case
  (:==) -> "="
  (:<=) -> "<="
  (:>=) -> ">="
  (:<)  -> "<"
  (:>)  -> ">"
  Like  -> error "'Like' is not suitable for showCmp"

data Cond (b::(Type,Schema)) (a::CondType) where
  Empty  :: Cond b a
  Cmp    :: (KnownSymbol a, FldCons (Fst b) v)
         => Tagged a (Cmp,v) -> Cond b TRec -- fld
  Null   :: KnownSymbol a => Proxy a -> Cond b TRec -- fld
  Not    :: Cond b a -> Cond b a
  BoolOp :: BoolOp -> Cond b a -> Cond b a -> Cond b a
  Child  :: (KnownSymbol a, GetRef a (Snd b) ~ Just ref)
         => Tagged a (Cond b TRec) -> Cond b TChild -- ref
  Tree   :: Cond b TRec -> Cond b TChild -> Cond b TTree

deriving instance Show (Cond b a)

instance ToJSON (Cond b a) where
  toJSON = \case
    Empty -> Object mempty
    Cmp (Tagged (c,v) :: Tagged s (Cmp,v))
      -> object ["fld" .= symbolVal (Proxy @s), "op" .= c, "val" .= v]
                -- , "type" .= show (typeOf b)]
    Null p -> object ["fld" .= symbolVal p, "null" .= True]
    Not x -> object ["not" .= True, "cond" .= x]
    BoolOp op c1 c2 -> object ["op" .= op, "c1" .= c1, "c2" .= c2]
    Child (Tagged c :: Tagged s (Cond b TRec)) ->
      object ["ref" .= symbolVal (Proxy @s), "rec" .= c]
    Tree c1 c2 -> object ["rec" .= c1, "childs" .= c2]

data CondTree b (a :: CondTreeType) where
  MainTree  :: KnownSymbol a => Tagged a (Cond b TTree) -> [CondTree b TTChild]
                             -> CondTree b TTMain
  ChildTree :: KnownSymbol a => Tagged a (Cond b TTree) -> [CondTree b TTChild]
                             -> CondTree b TTChild

deriving instance Show (CondTree b a)

instance ToJSON (CondTree b a) where
  toJSON  = \case
    MainTree (Tagged c :: Tagged s (Cond b TTree)) cts
      -> object ["tab" .= symbolVal (Proxy @s), "tree" .= c, "childs" .= cts]
    ChildTree (Tagged c :: Tagged s (Cond b TTree)) cts
      -> object ["ref" .= symbolVal (Proxy @s), "tree" .= c, "childs" .= cts]

-- newtype WithSchema (sch :: Schema) a = WithSchema a

instance (SingI sch, FldInfo sch b)
      => FromJSON (CondTree '(b,(sch::Schema)) TTMain) where
  parseJSON = withObject  "CondTree sch TTMain" $ \o -> do
    dsName <- o .: "tab"
    case dsByName dsName ssch of
      Nothing -> failName "DataStruct" dsName
      Just ds -> case someSymbolVal (unpack dsName) of
        SomeSymbol (_::Proxy s) -> MainTree
                                <$> (Tagged @s <$> parseTreeFld ds o "tree")
                                <*> parseChildsTreeFld dsName o "childs"
    where
      ssch = sing @_ @sch
      failName typ val = fail $ typ ++ " with name '" ++ unpack val
                       ++ "' is not found in Schema"

      parseTree ds = withObject "Cond TTree" $ \o ->
        Tree <$> parseRecFld ds o "rec" <*> parseChildFld ds o "rec"

      parseRecFld = explicitParseField . parseRec
      parseChildFld = explicitParseField . parseChild

      parseRec ds v = flip (withObject "Cond TRec") v
        $ \o -> if o == mempty then return Empty else do
          mfld <- o .:? "fld"
          mnt <- if isNothing mfld then o .:? "not" else return Nothing
          mbo <- if isNothing (mfld >> mnt) then o .:? "op" else return Nothing
          case (mfld,mnt,mbo) of
            (Just fld,_,_) ->
              case ( someSymbolVal $ unpack fld
                   , M.lookup (getDsName' ds, fld) (fldInfo @sch @b)
                   ) of
                (SomeSymbol p, Just (SomeFldType ft)) -> case (p,ft) of
                  (_ :: FldCons b fd => (Proxy s, Proxy fd)) -> do
                    nl <- o.:? "null"
                    if nl == Just True then return (Null p)
                      else fmap (Cmp . Tagged @s)
                              $ (,) @_ @fd <$> o .: "op" <*> o .: "val"
                (_, Nothing) -> failName "Field"
                                  (getDsName' ds `mappend` "." `mappend` fld)
            (_, Just True, _) -> Not <$> parseRecFld ds o "cond"
            (_, _, Just bo) -> BoolOp bo <$> parseRecFld ds o "c1"
                                         <*> parseRecFld ds o "c2"
            _ -> typeMismatch "Cond TRec" v

      parseChild ds v = flip (withObject "Cond TChild") v
        $ \o -> if o == mempty then return Empty else do
          mr <- o .:? "ref" -- Child
          mnt <- if isNothing mr then o .:? "not" else return Nothing
          mbo <- if isNothing (mr >> mnt) then o .:? "op" else return Nothing
          case (mr,mnt,mbo) of
            (Just ref,_,_) -> checkRefAndParseDs ref (getDsName' ds) $ \cds ->
              case someSymbolVal (unpack ref) of
                SomeSymbol (_::Proxy s) -> case (sGetRef (sing @_ @s) (sing @_ @sch)) of
                  SNothing -> failName "Reference" ref
                  SJust _  -> Child . Tagged @s <$> parseRecFld cds o "rec"
            (_,Just nt,_) -> (if nt then Not else id) <$> parseChildFld ds o "c"
            (_,_,Just bo) -> BoolOp bo <$> parseChildFld ds o "c1"
                                       <*> parseChildFld ds o "c2"
            _ -> typeMismatch "Cond TChild" v

      parseTreeFld ds o fldName = explicitParseField (parseTree ds) o fldName
      parseChildsTreeFld :: Text -> Object -> Text
                         -> Parser [CondTree '(b,sch) TTChild]
      parseChildsTreeFld dsName o fldName =
        explicitParseField (listParser $ parseChildTree dsName) o fldName

      checkRefAndParseDs :: forall r. Text -> Text
                         -> (SomeSing DataStruct -> Parser r) -> Parser r
      checkRefAndParseDs refn toName parser = case refByName refn ssch of
        Nothing -> failName "Reference" refn
        Just (SomeSing ref) -> case ref of
          SRefC { sRefFrom = rfrom, sRefTo = rto } ->
            if fromSing rto /= toName
              then fail $ "Reference with name " ++ unpack refn
                        ++ " is not referenced to DataStruct "++ unpack refn
              else case dsByName (fromSing rfrom) ssch of
                Nothing -> failName "DataStruct (from reference) " (fromSing rfrom)
                Just ds -> parser ds

      parseChildTree :: Text -> Value -> Parser (CondTree '(b,sch) TTChild)
      parseChildTree dsName = withObject "CondTree TTChild" $ \o -> do
        refn <- o .: "ref"
        checkRefAndParseDs refn dsName $ \ds ->
          case someSymbolVal (unpack refn) of
            SomeSymbol (_::Proxy s) -> ChildTree
              <$> (Tagged @s <$> parseTreeFld ds o "tree")
              <*> parseChildsTreeFld (getDsName' ds) o "childs"

class KnownSymbol a => CCond (a::Symbol) (b::(Type,Schema)) where
  pcmp   :: FldCons (Fst b) v => Cmp -> v -> Cond b TRec
  pchild :: GetRef a (Snd b) ~ Just ref => Cond b TRec -> Cond b TChild
  pnull  :: Cond b TRec
  pMainTree  :: Cond b TTree -> [CondTree b TTChild] -> CondTree b TTMain
  pChildTree :: Cond b TTree -> [CondTree b TTChild] -> CondTree b TTChild

instance KnownSymbol a => CCond a b where
  pcmp o v = Cmp (Tagged @a (o,v))
  pchild x = Child (Tagged @a x)
  pnull = Null (Proxy @a)
  pMainTree c cs = MainTree (Tagged @a c) cs
  pChildTree c cs = ChildTree (Tagged @a c) cs

pnot :: Cond b a -> Cond b a
pnot = Not

(&&&), (|||) :: Cond b a -> Cond b a -> Cond b a
(&&&) = BoolOp And
(|||) = BoolOp Or
infixl 2 |||
infixl 3 &&&

(<?),(>?),(<=?),(>=?),(==?),(~?) :: (Cmp -> v -> Cond b TRec) -> v -> Cond b TRec
x <? b  = x (:<)  b
x >? b  = x (:>)  b
x <=? b = x (:<=) b
x >=? b = x (:>=) b
x ==? b = x (:==) b
x ~? b  = x Like  b
infix 4 <?, >?, <=?, >=?, ==?, ~?

-- test (_::Proxy (b::(Type,Schema))) = Tree @b (pcmp @"val" >? (0 :: Double) ) Empty

test (_::Proxy b) = Tree @b (pcmp @"val" >? (0 :: Double) )
     $ pchild @"xxx" (pnot (pcmp @"x" >? 'x'
                        &&& pcmp @"x" <=? 'z'
                        &&& pcmp @"y" ~? 'z'
                        ||| pnull @"z"
                      ))
   &&& pchild @"zzz" (pnot (pcmp @"x" >? ("xxx"::Text) &&& pcmp @"x" <=? 'z'
                    ||| pnull @"z"))

-- номер таблицы родителя (номер дочерней таблицы, номер параметра)
type ConvCondMonad = ReaderT Int (State (Int, Int))
runConvCond :: ConvCondMonad a -> a
runConvCond x = evalState (runReaderT x 0) (0,0)

withPar :: DbOption b => Proxy b -> (Text -> a) -> ConvCondMonad a
withPar (_::Proxy b) f = lift $ do
  n <- snd <$> get
  modify $ second (+1)
  return $ f (paramName @b n)

convCond :: SingI sch => Cond '(b,sch) a -> ConvCondMonad (Text, [FieldDB b])
convCond (condition :: Cond '(b,sch) a) = case condition of
  Empty -> return ("1=1",[])
  (Cmp (Tagged (op,v) :: (KnownSymbol s, FldCons b v) => Tagged s (Cmp,v))) -> do
    ntab <- lift $ fst <$> get
    withPar (Proxy @b) $ \par
      -> let nm = formatS "t{}.{}" (ntab, symbolVal (Proxy @s)) in
           (case op of
             Like -> condLike @b nm par
             _    -> formatS "{}{}{}" (nm,showCmp op,par)
           , convert v)
  Null p -> return (pack (symbolVal p) `mappend` " IS NULL", [])
  Not c -> first (formatS "NOT ({})" . Only) <$> convCond c
  BoolOp bo c1 c2 ->
    (\(cc1,d1) (cc2,d2) -> (formatS "({}) {} ({})" (cc1,show bo,cc2),d1++d2))
      <$> convCond c1 <*> convCond c2
  Child (Tagged cond :: Tagged r (Cond '(b,sch) TRec)) -> do
    pnum <- ask
    cnum <- lift $ do
      modify (first (+1))
      fst <$> get
    case sGetRef (sing @_ @r) (sing @_ @sch) of
      SJust rf -> fmap (first (\c ->
            formatS "EXISTS (SELECT 1 FROM {} t{} WHERE {}{}{})"
              ( fromSing (sGetRefFrom rf)
              , cnum
              , T.intercalate " AND "
                  $ map (\(ch,pr) -> formatS "t{}.{} = t{}.{}" (cnum,ch,pnum,pr))
                  $ fromSing (sGetRefCols rf)
              , if T.null c then (""::T.Text) else " AND "
              , c)
          ))
        $ local (const cnum)
        $ convCond cond
  Tree cr cc -> (\(r,rs) (c,cs) -> (formatS "({}) AND ({})" (r,c), rs++cs))
             <$> convCond cr <*> convCond cc
