module Tests.Hanoi where

import Syntax

import FairEval
import FairStream
import Labels
import Embedding
import Util

import Program.Hanoi
import Program.HanoiBad

import qualified Unfolding as U

---------------------------------------

defs :: [Def]
defs = fst $ tree2defs hanoi

defs1 :: [Def]
defs1 = fst $ tree2defs hanoiBad


pins :: Int -> Tx
pins m = C "triple" [gen 0, C "nil" [], C "nil" []] where
  gen n = if n == m then C "nil" [] else C "%" [int2nat n, gen $ n + 1]

goal :: G X
goal = Invoke "check" [pins 3, V "answer", C "true" []]

esVars = [("check",      [1]   ), ("get",   []),
          ("one_step",   []    ), ("isNil", []),
          ("notEqStick", []    ), ("set",   []),
          ("less",       [0, 1])
         ]

vars = ["answer"]

goalUnit :: RunGoal X ()
goalUnit = RG goal

goalInt :: RunGoal X Int
goalInt  = RG goal

goalDisj :: RunGoal X (Disj, Int)
goalDisj = RG goal

goalEmbed :: RunGoal X Streams
goalEmbed = RG goal

goalInvEmbed :: RunGoal X StreamsDict
goalInvEmbed = RG goal

goalInvs :: RunGoal X InvokesDict
goalInvs = RG goal

goalDefs :: RunGoal X DefsLabel
goalDefs = RG goal

----------------------------------------------------
----------------------------------------------------
----------------------------------------------------

  -- first answer
  -- step  :     270000
  -- path  :         15
  -- height:         19
  -- size  :      25071
  -- disjs :       6870
  -- conjs :       5665
  -- actCnj:       1584
  -- d in c:         11
  -- maxLs :          0
  -- swaps :          0

testUnit =
  putStrLn $ show $ takeAnswers 1 $ run vars defs () goalUnit

----------------------------------------------------

  -- first answer
  -- step  :    1140000
  -- path  :         16
  -- height:         21
  -- size  :     209051
  -- disjs :      24850
  -- conjs :      79675
  -- actCnj:      20555
  -- d in c:          2
  -- maxLs :         14
  -- swaps :     107356
testShallowIgnoringEmbed =
  putStrLn $ show $ takeAnswers 1 $ run vars defs (sc2 shallowIgnoringEmbed eqAF) goalEmbed

----------------------------------------------------

  -- first answer
  -- step  :     970000
  -- path  :         17
  -- height:         21
  -- size  :     195089
  -- disjs :      22367
  -- conjs :      75177
  -- actCnj:      18359
  -- d in c:          2
  -- maxLs :         14
  -- swaps :      89731

testShallowestIgnoringSubformula =
  putStrLn $ show $ takeAnswers 1 $ run vars defs (sc2 shallowestIgnoringSubformula eqAF) goalEmbed

----------------------------------------------------

  -- first answer
  -- step  :     590000
  -- path  :         14
  -- height:         23
  -- size  :      58553
  -- disjs :      15794
  -- conjs :      13482
  -- actCnj:       3087
  -- d in c:         17
  -- maxLs :         26
  -- swaps :        596
testInvLeftSubformula =
  putStrLn $ show $ takeAnswers 1 $ run vars defs (cmpSD shallowestIgnoringSubformula) goalInvEmbed

----------------------------------------------------

  -- first answer
  -- step  :     270000
  -- path  :         15
  -- height:         19
  -- size  :      25071
  -- disjs :       6870
  -- conjs :       5665
  -- actCnj:       1584
  -- d in c:         11
  -- maxLs :         28
  -- swaps :          0

testInvLeftSubformulaCmpHeights =
  putStrLn $ show $ takeAnswers 1 $ run vars defs (cmpSD cmpHeightsIgnoringLeftSubformula) goalInvEmbed

----------------------------------------------------

  -- first answer
  -- step  :    1830000
  -- path  :         16
  -- height:         23
  -- size  :     382975
  -- disjs :      58835
  -- conjs :     132652
  -- actCnj:      31023
  -- d in c:         12
  -- maxLs :          8
  -- swaps :       5832

testInvsSubinvoke_NonStrict =
  putStrLn $ show $ takeAnswers 1 $ run vars defs NonStrict goalInvs

----------------------------------------------------

testInvsSubinvoke_Strict =
  putStrLn $ show $ takeAnswers 1 $ run vars defs Strict goalInvs

----------------------------------------------------

  -- first answer
  -- step  :     270000
  -- path  :         15
  -- height:         19
  -- size  :      25071
  -- disjs :       6870
  -- conjs :       5665
  -- actCnj:       1584
  -- d in c:         11
  -- maxLs :         28
  -- swaps :          0

testDefsApprox =
  putStrLn $ show $ takeAnswers 1 $ run vars defs (toDA defs) goalDefs

----------------------------------------------------
----------------------------------------------------
----------------------------------------------------

  -- 82328
testUnfoldSimpl =
  putStrLn $ show $ U.takeAnswers 1 $ U.run U.left2rightHandler vars defs goal

testUnfoldSimpl1 =
  putStrLn $ show $ U.takeAnswers 1 $ U.run U.left2rightHandler vars defs1 goal

testUnfoldSimplFair m =
  putStrLn $ show $ U.takeAnswers 1 $ U.run (U.naiveFairHandler m) vars defs goal

testUnfoldSimplFair1 m =
    putStrLn $ show $ U.takeAnswers 1 $ U.run (U.naiveFairHandler m) vars defs1 goal

  -- 19534
testUnfoldDefsRating =
  putStrLn $ show $ U.takeAnswers 1 $ U.run100 (U.defsRatingSep defs) vars defs goal

  -- 46217
testUnfoldFirstGoodCall =
  putStrLn $ show $ U.takeAnswers 1 $ U.run100 (U.firstGoodCallSep defs) vars defs goal

  -- 82328
testUnfoldEssentialArgs =
  putStrLn $ show $ U.takeAnswers 1 $ U.run100 (U.hasEssentialArgsSep esVars) vars defs goal

  -- 82328
testUnfoldEssentialArgs1 =
  putStrLn $ show $ U.takeAnswers 1 $ U.run100 (U.hasEssentialArgsSep esVars) vars defs1 goal

  -- 82328
testUnfoldingFairConj =
  putStrLn $ show $ U.takeAnswers 1 $ U.run100 (U.fairConj defs esVars) vars defs goal

  -- 82328
testUnfoldEmbed =
  putStrLn $ show $ U.takeAnswers 1 $ U.run U.embedHandler vars defs $ goal

  -- 82491
testUnfoldEmbed1 =
  putStrLn $ show $ U.takeAnswers 1 $ U.run U.embedHandler vars defs1 $ goal

  -- 281056
testUnfoldEmbedBackward =
  putStrLn $ show $ U.takeAnswers 1 $ U.run U.embedBackwardHandler vars defs $ goal

  -- 2634875
testUnfoldEmbedBackward1 =
  putStrLn $ show $ U.takeAnswers 1 $ U.run U.embedBackwardHandler vars defs1 $ goal

  -- 82328
testUnfoldEssentialHeight =
  putStrLn $ show $ U.takeAnswers 1 $ U.run (U.essentialHeightHandler esVars) vars defs $ goal

  -- 82328
testUnfoldEssentialHeight1 =
  putStrLn $ show $ U.takeAnswers 1 $ U.run (U.essentialHeightHandler esVars) vars defs1 $ goal
