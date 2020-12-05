module Embedding where

import Syntax

import FairStream
import Util

---------------------------------------

embedT :: Ts -> Ts -> Bool
embedT (V _) (V _) = True
embedT a     b     = couple a b || diving a b where
  couple (C n a) (C m b) | n == m && length a == length b = all (uncurry embedT) $ zip a b
  couple _ _ = False
  diving t (C _ a) = any (embedT t) a
  diving _ _ = False

shallowEmbedG :: Goal -> Goal -> Bool
shallowEmbedG a b = couple a b || diving a b where
  couple (_  :=:  _ ) (_   :=:  _  ) = True
  couple (Invoke n _) (Invoke m _  ) = n == m
  couple (g1 :/\: g2) (g1' :/\: g2') = shallowEmbedG g1 g1' && shallowEmbedG g2 g2'
  couple (g1 :\/: g2) (g1' :\/: g2') = shallowEmbedG g1 g1' && shallowEmbedG g2 g2'
  couple _ _ = False
  diving g (g1 :/\: g2) = shallowEmbedG g g1 || shallowEmbedG g g2
  diving g (g1 :\/: g2) = shallowEmbedG g g1 || shallowEmbedG g g2
  diving _ _ = False

embedG :: Goal -> Goal -> Bool
embedG a b = couple a b || diving a b where
  couple (t1 :=:  t2) (t1' :=:  t2') = embedT t1 t1' && embedT t2 t2'
  couple (g1 :/\: g2) (g1' :/\: g2') = embedG g1 g1' && embedG g2 g2'
  couple (g1 :\/: g2) (g1' :\/: g2') = embedG g1 g1' && embedG g2 g2'
  couple (Invoke n a) (Invoke n' a') | n == n' && length a == length a' = all (uncurry embedT) $ zip a a'
  couple _ _ = False
  diving g (g1 :/\: g2) = embedG g g1 || embedG g g2
  diving g (g1 :\/: g2) = embedG g g1 || embedG g g2
  diving _ _ = False

---------------------------------------

data FunName = Uni | Fun String deriving Eq

data AbstractFormula = Call FunName [Ts]
                     | AbstractFormula :&: AbstractFormula
                     | AbstractFormula :|: AbstractFormula deriving Eq

goalToAF :: Goal -> AbstractFormula
goalToAF (t1 :=: t2)  = Call Uni [t1, t2]
goalToAF (Invoke n a) = Call (Fun n) a
goalToAF (a :/\: b)   = goalToAF a :&: goalToAF b
goalToAF (a :\/: b)   = goalToAF a :|: goalToAF b

streamToAF :: GenStream s l -> AbstractFormula
streamToAF (Goal g _)   = goalToAF g
streamToAF (Disj a b)   = streamToAF a :|: streamToAF b
streamToAF (Conj a _ b) = streamToAF a :&: streamToAF b

streamToDeepAF :: Stream l -> AbstractFormula
streamToDeepAF = streamToAF . substInS

eqAF :: GenStream s l -> GenStream s l -> Bool
eqAF a b = streamToAF a == streamToAF b

---------------------------------------

-- ignores terms and substitutions
-- does not distinguish between syntactic and semantic operators
shallowestIgnoringEmbed :: Stream l -> Stream l -> Bool
shallowestIgnoringEmbed a b = embed (streamToAF a) $ streamToAF b where
  embed a b = couple a b || diving a b
  couple (Call n a) (Call m b) = n == m && length a == length b
  couple (a :&: b) (a' :&: b') = embed a a' && embed b b'
  couple (a :|: b) (a' :|: b') = embed a a' && embed b b'
  couple _         _           = False
  diving x (a :&: b) = embed x a || embed x b
  diving x (a :|: b) = embed x a || embed x b
  diving _ _         = False

-- ignores only substitutions
-- does not distinguish between syntactic and semantic operators
shallowIgnoringEmbed :: Stream l -> Stream l -> Bool
shallowIgnoringEmbed a b = embed (streamToAF a) $ streamToAF b where
  embed a b = couple a b || diving a b
  couple (Call n a) (Call m b) = n == m && length a == length b && all (uncurry embedT) (zip a b)
  couple (a :&: b) (a' :&: b') = embed a a' && embed b b'
  couple (a :|: b) (a' :|: b') = embed a a' && embed b b'
  couple _         _           = False
  diving x (a :&: b) = embed x a || embed x b
  diving x (a :|: b) = embed x a || embed x b
  diving _ _         = False

-- does not distinguish between syntactic and semantic operators
deepIgnoringEmbed :: Stream l -> Stream l -> Bool
deepIgnoringEmbed a b = embed (streamToDeepAF a) $ streamToDeepAF b where
  embed a b = couple a b || diving a b
  couple (Call n a) (Call m b) = n == m && length a == length b && all (uncurry embedT) (zip a b)
  couple (a :&: b) (a' :&: b') = embed a a' && embed b b'
  couple (a :|: b) (a' :|: b') = embed a a' && embed b b'
  couple _         _           = False
  diving x (a :&: b) = embed x a || embed x b
  diving x (a :|: b) = embed x a || embed x b
  diving _ _         = False

-- ignores terms and substitutions
-- distinguishes between syntactic and semantic operator
shallowestEmbed :: GenStream s l -> GenStream s' l' -> Bool
shallowestEmbed a b = couple a b || diving a b where
  couple (Goal a _)   (Goal b _)   = shallowEmbedG a b
  couple (Disj a b)   (Disj c d)   = shallowestEmbed a c && shallowestEmbed b d
  couple (Conj a _ b) (Conj c _ d) = shallowestEmbed a c && shallowestEmbed b d
  couple _            _            = False
  diving x (Disj a b)   = shallowestEmbed x a || shallowestEmbed x b
  diving x (Conj a _ b) = shallowestEmbed x a || shallowestEmbed x b
  diving _ _            = False

---------------------------------------

shallowestIgnoringSubformula :: Stream l -> Stream l -> Bool
shallowestIgnoringSubformula a b = subf (streamToAF a) $ streamToAF b where
  subf a b = couple a b || diving a b
  couple (Call n a) (Call m b) = n == m && length a == length b
  couple (a :&: b) (a' :&: b') = couple a a' && couple b b'
  couple (a :|: b) (a' :|: b') = couple a a' && couple b b'
  couple _         _           = False
  diving x (a :&: b) = subf x a || subf x b
  diving x (a :|: b) = subf x a || subf x b
  diving _ _         = False

shallowestIgnoringLeftSubformula :: Stream l -> Stream l -> Bool
shallowestIgnoringLeftSubformula a b = subf (streamToAF a) $ streamToAF b where
  subf a b = couple a b || diving a b
  couple (Call n a) (Call m b) = n == m && length a == length b
  couple (a :&: b) (a' :&: b') = couple a a' && couple b b'
  couple (a :|: b) (a' :|: b') = couple a a' && couple b b'
  couple _         _           = False
  diving x (a :&: _) = subf x a
  diving x (a :|: _) = subf x a
  diving _ _         = False

cmpHeightsIgnoringLeftSubformula :: Stream l -> Stream l -> Bool
cmpHeightsIgnoringLeftSubformula a b = subf (streamToDeepAF a) $ streamToDeepAF b where
  subf a b = couple a b || diving a b
  couple (Call n a) (Call m b) = n == m && length a == length b &&
                                 maximum (0 : map heightT a) == maximum (0 : map heightT b)
  couple (a :&: b) (a' :&: b') = couple a a' && couple b b'
  couple (a :|: b) (a' :|: b') = couple a a' && couple b b'
  couple _         _           = False
  diving x (a :&: _) = subf x a
  diving x (a :|: _) = subf x a
  diving _ _         = False
