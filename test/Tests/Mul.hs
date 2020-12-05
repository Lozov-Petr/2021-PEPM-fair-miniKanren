module Tests.Mul where

import Prelude hiding (succ)

import Syntax

import FairEval
import FairStream
import Labels
import Embedding

import Program.Num

import qualified Unfolding as U

defs :: [Def]
defs = mulo

goal1 :: G X
goal1 = Invoke "mulo" [V "a", V "b", succ zero]

goal2 :: G X
goal2 = fresh ["x"] $ Invoke "mulo" [V "a", V "b", V "x"] &&& Invoke "mulo" [V "x", V "c", succ zero]

vars1 = ["a", "b"]
vars2 = ["a", "b", "c"]

goal1Unit :: RunGoal X ()
goal1Unit = RG goal1

goal2Unit :: RunGoal X ()
goal2Unit = RG goal2

testUnit1 =
  putStrLn $ show $ takeAnswers 2 $ run vars1 defs () goal1Unit

testUnit2 =
  putStrLn $ show $ run vars2 defs () goal1Unit
