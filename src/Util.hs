module Util where

import Syntax
import FairStream

import qualified Eval

---------------------------------------

unify :: Subst -> Ts -> Ts -> Maybe Subst
unify (n, s) t1 t2 = Eval.unify (Just s) t1 t2 >>= return . ((,) n)

---------------------------------------

getLeftLeaf :: GenStream s l -> (Goal, s)
getLeftLeaf (Disj a _  ) = getLeftLeaf a
getLeftLeaf (Conj a _ _) = getLeftLeaf a
getLeftLeaf (Goal g s) = (g, s)

height :: GenStream a b -> Int
height (Conj a _ b) = 1 + max (height a) (height b)
height (Disj a b  ) = 1 + max (height a) (height b)
height _            = 0

heightT :: Term a -> Int
heightT (C _ a@(_:_)) = 1 + maximum (map heightT a)
heightT _             = 0

path :: GenStream a b -> Int
path (Conj a _ _) = 1 + path a
path (Disj a _  ) = 1 + path a
path _            = 0

sizeStream :: GenStream a b -> Int
sizeStream (Conj a _ b) = 1 + sizeStream a + sizeStream b
sizeStream (Disj a b)   = 1 + sizeStream a + sizeStream b
sizeStream _            = 1

disjCount :: GenStream a b -> Int
disjCount (Conj a _ _) = disjCount a
disjCount (Disj a b)   = 1 + disjCount a + disjCount b
disjCount _            = 0

conjCount :: GenStream a b -> Int
conjCount (Conj a _ b) = 1 + conjCount a + conjCount b
conjCount (Disj a b)   = conjCount a + conjCount b
conjCount _            = 0

disjsInConjs :: GenStream a b -> [Int]
disjsInConjs (Conj a _ _) = [disjCount a]
disjsInConjs (Disj a b)   = disjsInConjs a ++ disjsInConjs b
disjsInConjs _            = []

termHeight :: Subst -> Ts -> Int
termHeight s t = max 1 $ height $ substInT s t where
  height (V _)   = 0
  height (C _ a) = 1 + foldr (max . height) 0 a

substInT :: Subst -> Ts -> Ts
substInT s (V x) =
  case Eval.sLookup x $ snd s of
    Nothing -> V x
    Just t  -> substInT s t
substInT s (C n a) = C n $ map (substInT s) a

substInG :: Subst -> Goal -> Goal
substInG s (t1 :=: t2)  = substInT s t1 :=: substInT s t2
substInG s (Invoke n a) = Invoke n $ map (substInT s) a
substInG s (a :\/: b)   = substInG s a :\/: substInG s b
substInG s (a :/\: b)   = substInG s a :/\: substInG s b

substInS :: Stream l -> HoleStream l
substInS (Goal g s)   = Goal (substInG s g) ()
substInS (Disj a b)   = Disj (substInS a) $ substInS b
substInS (Conj a l b) = Conj (substInS a) l b

maxSizeLabels :: Labels l p => p -> GenStream a l -> Int
maxSizeLabels p (Conj a l b) = maximum [size p l, maxSizeLabels p a, maxSizeLabels p b]
maxSizeLabels p (Disj a b)   = max (maxSizeLabels p a) $ maxSizeLabels p b
maxSizeLabels _ _            = 0

tree2defs :: G X -> ([Def], G X)
tree2defs (a :\/: b) =
  let (ds,  a') = tree2defs a in
  let (ds', b') = tree2defs b in
  (ds ++ ds', a' :\/: b')
tree2defs (a :/\: b) =
  let (ds,  a') = tree2defs a in
  let (ds', b') = tree2defs b in
  (ds ++ ds', a' :/\: b')
tree2defs (Fresh a g) =
  let (ds, g') = tree2defs g in
  (ds, Fresh a g')
tree2defs (Let (Def n a b) r) =
  let (ds,  b') = tree2defs b in
  let (ds', r') = tree2defs r in
  (Def n a b' : ds ++ ds', r')
tree2defs a           = ([], a)

---------------------------------------

int2nat 0 = C "o" []
int2nat n = C "s" [int2nat $ n - 1]
