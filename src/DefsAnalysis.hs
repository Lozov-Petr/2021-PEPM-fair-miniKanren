module DefsAnalysis where

import Syntax
import Eval

import Util hiding (unify)

---------------------------------------

type Approx = (Name, [[Ts]])

---------------------------------------

def2approx :: Def -> Approx
def2approx (Def n a b) = (n, map (\(s,_,i) -> map (substInT (i,s) . V) [(-1),(-2)..(-l)]) $ toSubsts sEmpty (zip a [-1,-2..]) (-l-1) b) where
  l = length a
  toSubsts :: MapSigma -> [(X, S)] -> Int -> G X -> [(MapSigma, [(X, S)], Int)]
  toSubsts s env i (Invoke _ _) = [(s, env, i)]
  toSubsts s env i (a :=: b)    = case unify (Just s) (app env a) (app env b) of
                                    Nothing -> []
                                    Just s  -> [(s, env, i)]
  toSubsts s env i (a :/\: b)   = toSubsts s env i a >>= \(s, env, i) -> toSubsts s env i b
  toSubsts s env i (a :\/: b)   = toSubsts s env i a ++ toSubsts s env i b
  toSubsts s env i (Fresh a g)  = toSubsts s ((a, i) : env) (i-1) g
  app env (C n a) = C n $ map (app env) a
  app env (V x)   = case lookup x env of
                      Just a  -> V a
                      Nothing -> error "Unexpected variable"
