module Tests.GCW where

import Syntax

import FairEval
import FairStream
import Labels
import Embedding
import Util

import Program.GCW

import qualified Unfolding as U

---------------------------------------

defs :: [Def]
defs = fst $ tree2defs gcw

goal :: G X
goal = Invoke "checkAnswer" [V "answer", C "true" []]

vars = ["answer"]

goalUnit :: RunGoal X ()
goalUnit = RG goal

goalInt :: RunGoal X Int
goalInt  = RG goal

goalDisj :: RunGoal X (Disj, Int)
goalDisj = RG goal

goalEmbed :: RunGoal X Streams
goalEmbed = RG goal

----------------------------------------------------
----------------------------------------------------
----------------------------------------------------

  -- 100 answers
  -- step  :    1310000
  -- path  :         15
  -- height:         19
  -- size  :      74483
  -- disjs :      20821
  -- conjs :      16420
  -- actCnj:       9348
  -- d in c:          5
  -- maxLs :          0
  -- swaps :          0

testUnit =
  putStrLn $ show $ takeAnswers 100 $ run vars defs () goalUnit

----------------------------------------------------

testShallowesIgnoringSubformula =
  putStrLn $ show $ takeAnswers 100 $ run vars defs (sc2 shallowestIgnoringSubformula eqAF) goalEmbed

----------------------------------------------------
----------------------------------------------------
----------------------------------------------------

testUnfoldSimpl =
  putStrLn $ show $ U.takeAnswers 100 $ U.run U.left2rightHandler vars defs goal

testUnfoldDefsRating =
  putStrLn $ show $ U.takeAnswers 100 $ U.run100 (U.defsRatingSep defs) vars defs goal

testUnfoldFirstGoodCall =
  putStrLn $ show $ U.takeAnswers 100 $ U.run100 (U.firstGoodCallSep defs) vars defs goal
