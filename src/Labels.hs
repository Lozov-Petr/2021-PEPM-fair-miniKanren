{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Labels where

import Data.Maybe

import Syntax
import qualified Embed

import Util
import FairStream
import DefsAnalysis

---------------------------------------

instance (Labels l1 p1, Labels l2 p2) => Labels (l1, l2) (p1, p2) where
  new (p1, p2) s = (new p1 $ fmap fst s, new p2 $ fmap snd s)
  keep (p1, p2) (l1, l2) = (keep p1 l1, keep p2 l2)
  predicate (p1, p2) s (l1, l2) = predicate p1 (fmap fst s) l1 && predicate p2 (fmap snd s) l2
  update (p1, p2) s1 s2 l (l1, l2) = (update p1 (fmap fst s1) (fmap fst s2) l l1,
                                    update p2 (fmap snd s1) (fmap snd s2) l l2)
  size (p1, p2) (l1, l2) = size p1 l1 + size p2 l2

---------------------------------------

instance Labels () () where
  new () _ = ()
  keep () () = ()
  predicate () _ () = True
  update () _ _ _ () = ()
  size _ _ = 0

---------------------------------------

instance Labels Int Int where
  new i _ = i
  keep _ n = n
  predicate _ _ n = n /= 0
  update _ _ _ _ n = n - 1
  size _ _ = 0

---------------------------------------

newtype Invokes = I Int deriving Show

instance Labels Invokes Invokes where
  new i _ = i
  keep _ i = i
  predicate _ _ (I i) = i /= 0
  update _ _ _ l x@(I i) =
    case last l of
      InvokeStep _ -> I $ i - 1
      _            -> x
  size _ _ = 0
---------------------------------------

newtype Disj = D Int deriving Show

disjs (Conj _ (D d) _) = d
disjs (Disj a b)       = 1 + disjs a + disjs b
disjs _                = 0

instance Labels Disj Disj where
  new _ s = D $ disjs s
  keep _ _ = D 0
  predicate (D p) _ (D d) = d <= p
  update  _  _ s _ _ = D $ disjs s
  size _ _ = 0

---------------------------------------

data SignVars  = SV [(Int, Ts)] Int deriving Show
data SignVarsP = SVP [Int] Int

-- It desn't work
instance Labels SignVars SignVarsP where
  new (SVP l n) s = SV (map (\v -> (v, substInT a $ V v)) l) n where
    (_, a) = getLeftLeaf s
  keep _ l = l
  predicate _ s (SV v i) = i /= 0 || any (\(i, t) -> Embed.isStrictInst t $ substInT a $ V i) v where
    (_, a) = getLeftLeaf s
  update (SVP l n) _ s _ (SV _ 0) = SV (map (\v -> (v, substInT a $ V v)) l) n where
    (_, a) = getLeftLeaf s
  update _ _ _ _ (SV l n) = SV l (n-1)
  size _ (SV l _) = length l

---------------------------------------

newtype Streams = Streams [Stream Streams] deriving Show
type Comp a = Stream a -> Stream a -> Bool
data Comparator a = SC (Comp a) (Maybe (Comp a))

sc1 :: Comp Streams -> Comparator Streams
sc1 = flip SC Nothing
sc2 :: Comp Streams -> Comp Streams -> Comparator Streams
sc2 f = SC f . Just

instance Labels Streams (Comparator Streams) where
  new _ _ = Streams []
  keep _ _ = Streams []
  predicate (SC f (Just g)) s (Streams ss@(x:y:_)) | g x y = not $ any (flip f s) ss
  predicate (SC _ (Just g)) s (Streams (x:_)) | g s x = True
  predicate (SC f _) s (Streams ss) = not $ any (flip f s) ss
  update _ s _ _ (Streams xs) = Streams $ s:xs
  size _ (Streams ss) = length ss

---------------------------------------

newtype StreamsDict = SD [(Name, Stream StreamsDict)] deriving Show

cmpSD :: Comp StreamsDict -> Comp StreamsDict
cmpSD = id

instance Labels StreamsDict (Comp StreamsDict) where
  new _ _ = SD []
  keep _ _ = SD []
  predicate c s (SD l) =
    case getLeftLeaf s of
      (Invoke n _, _) -> not $ any (flip c s) $ map snd $ filter ((n==) . fst) l
      _               -> True
  update _ s _ logs l@(SD xs) =
    case last logs of
      InvokeStep n -> SD $ (n, s) : xs
      _            -> l
  size _ (SD xs) = length xs

---------------------------------------

newtype InvokesDict = ID [(Name, [Ts])] deriving Show
data InvokesType = Strict | NonStrict

instance Labels InvokesDict InvokesType where
  new _ _  = ID []
  keep _ s = s
  predicate _ s (ID l) =
    case getLeftLeaf s of
      (Invoke n a, subst) -> not $ any (\(m, b) -> n == m && all (uncurry Embed.isInst) (zip b $ map (substInT subst) a)) l
      _                   -> True
  update Strict s _ _ l@(ID xs) =
    case s of
      (Goal (Invoke n a) subst) -> ID $ (n, map (substInT subst) a) : xs
      _                         -> l
  update NonStrict s _ _ l@(ID xs) =
    case getLeftLeaf s of
      (Invoke n a, subst) -> ID $ (n, map (substInT subst) a) : xs
      _                   -> l
  size _ (ID xs) = length xs

---------------------------------------

newtype DefsApprox = DA [(Name, [[Ts]])]
newtype DefsLabel = DL Bool deriving Show

toDA :: [Def] -> DefsApprox
toDA = DA . map def2approx

instance Labels DefsLabel DefsApprox where
  new _ _ = DL True
  keep _ x = x
  predicate (DA l) s (DL x) = x ||
    case getLeftLeaf s of
      (Invoke n a, subst) ->
        case lookup n l of
          Just ts -> length ts < 2 || not (all isJust $ map (\t -> foldl (\s (t1,t2) -> s >>= \s -> unify s t1 t2) (Just subst) $ zip a t) ts)
          Nothing -> error "Undefined definition."
      _ -> True
  update _ _ _ logs x =
    case last logs of
      InvokeStep _ -> DL False
      _            -> x
  size _ _ = 0
