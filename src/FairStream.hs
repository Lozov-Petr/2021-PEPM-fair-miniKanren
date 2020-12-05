{-# LANGUAGE MultiParamTypeClasses #-}

module FairStream where

import Text.Printf

import Syntax
import qualified Eval

---------------------------------------

data Log = Unify
         | DisjIntro
         | ConjIntro
         | InvokeStep Name
         | ConjSwap
         | ConjStop
         | ConjStopAns
         | ConjStep
         | ConjStepAns
         | DisjStop
         | DisjStopAns
         | DisjStep
         | DisjStepAns deriving Eq

type Logs = [Log]

---------------------------------------

type Er a = Either String a

type Goal  = G S
type Subst = (S, Eval.MapSigma)
type Hole  = ()
type Fun   = (Name, ([Name], G X))

data GenStream subst label
  = Goal Goal subst
  | Disj (GenStream subst label) (GenStream subst label)
  | Conj (GenStream subst label) label (GenStream Hole label)

type HoleStream l = GenStream Hole l
type Stream     l = GenStream Subst l

newtype RunGoal x l = RG (G x)

instance (Show s, Show l) => Show (GenStream s l) where
  show (Goal g s) = printf "<%s, %s>" (show g) $ show s
  show (Disj p q)   = printf "(%s |+| %s)" (show p) $ show q
  show (Conj s l g) = printf "(%s |*|{%s} %s)" (show s) (show l) $ show g

instance Functor (GenStream s) where
  fmap f (Conj a l b) = Conj (fmap f a) (f l) (fmap f b)
  fmap f (Disj a b)   = Disj (fmap f a) (fmap f b)
  fmap f (Goal g s)   = Goal g s

---------------------------------------

class Labels l p where
  new       :: p -> Stream l -> l
  keep      :: p -> l -> l
  predicate :: p -> Stream l -> l -> Bool
  update    :: p -> Stream l -> Stream l -> Logs -> l -> l
  size      :: p -> l -> Int
