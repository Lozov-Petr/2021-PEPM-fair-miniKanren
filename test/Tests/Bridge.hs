module Tests.Bridge where

import Syntax

import FairEval
import FairStream
import Labels
import Embedding

import Program.Bridge

import qualified Unfolding as U

----------------------------------------------------

defs :: [Def]
defs = game2Big

defs' :: [Def]
defs' = game2Big'

goal =
    Invoke "result" [V "minutes"] &&&
    Invoke "getAnswer" [V "answer", C "some" [V "minutes"]]

esVars = [("result",     []    ), ("getAnswer",   [0]   ),
          ("start",      []    ), ("getAnswer'",  [0]   ),
          ("checkStep",  []    ), ("checkPerson", []    ),
          ("eqForBool",  []    ), ("step",        []    ),
          ("movePerson", []    ), ("moveLight",   []    ),
          ("getTime",    []    ), ("grForPerson", []    ),
          ("times",      []    ), ("add",         [0, 2]),
          ("max",        [0, 1]), ("greater",     [0, 1]),
          ("finish",     []    ), ("eqForState",  []    )
         ]

vars = ["minutes", "answer"]

goalUnit :: RunGoal X ()
goalUnit = RG goal

goalInt :: RunGoal X Int
goalInt  = RG goal

goalDisj :: RunGoal X (Disj, Int)
goalDisj = RG goal

goalSignVars :: RunGoal X (SignVars, Int)
goalSignVars = RG goal

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
  -- step  :   36060000
  -- path  :         36
  -- height:         51
  -- size  :    2583373
  -- disjs :     545489
  -- conjs :     746197
  -- actCnj:         10
  -- d in c:     262993
  -- maxLs :          0
  -- swaps :          0

testUnit =
  putStrLn $ show $ takeAnswers 1 $ run vars defs () goalUnit

----------------------------------------------------

  -- steps: 3743291
  -- swaps: 0
  -- time:  26.647

testUnit' =
  putStrLn $ show $ takeAnswers 1 $ run vars defs' () goalUnit

----------------------------------------------------

  -- first answer
  -- step  :    4070000
  -- path  :         17
  -- height:         24
  -- size  :     215985
  -- disjs :      26365
  -- conjs :      81627
  -- actCnj:      13106
  -- d in c:         11
  -- maxLs :          0
  -- swaps :      31813

testInt100 =
  putStrLn $ show $ takeAnswers 1 $ run vars defs (100 :: Int) goalInt

testInt100' =
  putStrLn $ show $ takeAnswers 1 $ run vars defs' (100 :: Int) goalInt

----------------------------------------------------

  -- first answer
  -- step  :    1420000
  -- path  :         16
  -- height:         24
  -- size  :      91699
  -- disjs :      13839
  -- conjs :      32010
  -- actCnj:       6504
  -- d in c:         11
  -- maxLs :          0
  -- swaps :       1158

testDisj10 =
  putStrLn $ show $ takeAnswers 1 $ run vars defs (D 10, 10000 :: Int) goalDisj

----------------------------------------------------

  -- It doesn't work

testSignVars =
  putStrLn $ show $ takeAnswers 1 $ run vars defs (SVP [0, 1] 100, 10000 :: Int) goalSignVars

----------------------------------------------------

  -- first answer
  -- step  :    6250000
  -- path  :         21
  -- height:         27
  -- size  :     639561
  -- disjs :      52941
  -- conjs :     266839
  -- actCnj:      42533
  -- d in c:          2
  -- maxLs :         11
  -- swaps :    1056625

testShallowestIgnoringEmbed =
  putStrLn $ show $ takeAnswers 1 $ run vars defs (sc2 shallowestIgnoringEmbed eqAF) goalEmbed

----------------------------------------------------

  -- first answer
  -- step  :    6260000
  -- path  :         14
  -- height:         27
  -- size  :     637875
  -- disjs :      53007
  -- conjs :     265930
  -- actCnj:      41209
  -- d in c:          2
  -- maxLs :         11
  -- swaps :    1055534

testShallowIngoringEmbed =
  putStrLn $ show $ takeAnswers 1 $ run vars defs (sc2 shallowIgnoringEmbed eqAF) goalEmbed

----------------------------------------------------

  -- !!! Really slow: deep embedding is quite expensive

testDeepIgnoringEmbed =
  putStrLn $ show $ takeAnswers 1 $ run vars defs (sc2 deepIgnoringEmbed eqAF) goalEmbed

----------------------------------------------------

  -- first answer
  -- step  :    4470000
  -- path  :         21
  -- height:         25
  -- size  :     376563
  -- disjs :      31817
  -- conjs :     156464
  -- actCnj:      25733
  -- d in c:          2
  -- maxLs :         11
  -- swaps :     742304

testShallowestIgnoringSubformula =
  putStrLn $ show $ takeAnswers 1 $ run vars defs (sc2 shallowestIgnoringSubformula eqAF) goalEmbed

----------------------------------------------------

  -- first answer
  -- step  :    5650000
  -- path  :         18
  -- height:         26
  -- size  :     541041
  -- disjs :      43938
  -- conjs :     226582
  -- actCnj:      36154
  -- d in c:          2
  -- maxLs :         11
  -- swaps :     798299

testShallowestIgnoringLeftSubformula =
  putStrLn $ show $ takeAnswers 1 $ run vars defs (sc2 shallowestIgnoringLeftSubformula eqAF) goalEmbed

----------------------------------------------------

  -- first answer
  -- step  :   14990000
  -- path  :         24
  -- height:         32
  -- size  :    2639769
  -- disjs :     270283
  -- conjs :    1049601
  -- actCnj:      81624
  -- d in c:       1089
  -- maxLs :       3763
  -- swaps :     336183

testInvLeftSubformula =
  putStrLn $ show $ takeAnswers 1 $ run vars defs (cmpSD shallowestIgnoringLeftSubformula) goalInvEmbed

----------------------------------------------------
  -- first answer
--   step  :   16230000
--   path  :         26
--   height:         31
--   size  :    2427047
--   disjs :     273148
--   conjs :     940375
--   actCnj:      61859
--   d in c:       1307
--   maxLs :       6131
--   swaps :     110939

testInvLeftSubformulaCmpHeights =
  putStrLn $ show $ takeAnswers 1 $ run vars defs (cmpSD cmpHeightsIgnoringLeftSubformula) goalInvEmbed

----------------------------------------------------

testInvLeftSubformulaCmpHeights' =
  putStrLn $ show $ takeAnswers 1 $ run vars defs' (cmpSD cmpHeightsIgnoringLeftSubformula) goalInvEmbed

----------------------------------------------------

  -- first answer
  -- step  :    1240000
  -- path  :         11
  -- height:         24
  -- size  :     136489
  -- disjs :      16685
  -- conjs :      51559
  -- actCnj:       7432
  -- d in c:         15
  -- maxLs :         20
  -- swaps :      11882

testInvsSubinvoke_NonStrict =
  putStrLn $ show $ takeAnswers 1 $ run vars defs NonStrict goalInvs

----------------------------------------------------

  -- first step
  -- step: 1882850
  -- swap: 34060
  -- time: 0m14,438s

testInvsSubinvoke'_NonStrict =
  putStrLn $ show $ takeAnswers 1 $ run vars defs' NonStrict goalInvs

----------------------------------------------------

  -- first step
  -- step: 11996551
  -- swap: 4252
  -- time: 4m34,295s

testInvsSubinvoke_Strict =
  putStrLn $ show $ takeAnswers 1 $ run vars defs Strict goalInvs

----------------------------------------------------

  -- first step
  -- step: 3743291
  -- swap: 0
  -- time: 0m21,190s

testInvsSubinvoke'_Strict =
  putStrLn $ show $ takeAnswers 1 $ run vars defs' Strict goalInvs

----------------------------------------------------

  -- first answer
  -- step  :    1410000
  -- path  :         20
  -- height:         23
  -- size  :      71971
  -- disjs :       9000
  -- conjs :      26985
  -- actCnj:       4483
  -- d in c:         12
  -- maxLs :          0
  -- swaps :      10967

testDefsApprox =
  putStrLn $ show $ takeAnswers 1 $ run vars defs (toDA defs) goalDefs

----------------------------------------------------

testDefsApprox' =
  putStrLn $ show $ takeAnswers 1 $ run vars defs' (toDA defs') goalDefs

----------------------------------------------------
----------------------------------------------------
----------------------------------------------------

  -- 166505
testUnfoldSimpl =
  putStrLn $ show $ U.takeAnswers 1 $ U.run U.left2rightHandler vars defs goal

  -- 21689
testUnfoldSimpl' =
  putStrLn $ show $ U.takeAnswers 1 $ U.run U.left2rightHandler vars defs' goal

testUnfoldSimplFair m =
  putStrLn $ show $ U.takeAnswers 1 $ U.run (U.naiveFairHandler m) vars defs goal

testUnfoldSimplFair' m =
  putStrLn $ show $ U.takeAnswers 1 $ U.run (U.naiveFairHandler m) vars defs' goal

  -- did not wait for an answer
testUnfoldDefsRating =
  putStrLn $ show $ U.takeAnswers 1 $ U.run100 (U.defsRatingSep defs) vars defs goal

  -- did not wait for an answer
testUnfoldDefsRating' =
  putStrLn $ show $ U.takeAnswers 1 $ U.run100 (U.defsRatingSep defs) vars defs' goal

  -- 160729
testUnfoldFirstGoodCall =
  putStrLn $ show $ U.takeAnswers 1 $ U.run100 (U.firstGoodCallSep defs) vars defs goal

  -- 129377
testUnfoldFirstGoodCall' =
  putStrLn $ show $ U.takeAnswers 1 $ U.run100 (U.firstGoodCallSep defs) vars defs' goal

  -- 23179
testUnfoldEssentialArgs =
  putStrLn $ show $ U.takeAnswers 1 $ U.run100 (U.hasEssentialArgsSep esVars) vars defs goal

  -- 21689
testUnfoldEssentialArgs' =
  putStrLn $ show $ U.takeAnswers 1 $ U.run100 (U.hasEssentialArgsSep esVars) vars defs' goal

  -- 48335
testUnfoldingFairConj =
  putStrLn $ show $ U.takeAnswers 1 $ U.run100 (U.fairConj defs esVars) vars defs goal

  -- 20589
testUnfoldingFairConj' =
  putStrLn $ show $ U.takeAnswers 1 $ U.run100 (U.fairConj defs esVars) vars defs' goal

  -- 166505
testUnfoldEmbed =
  putStrLn $ show $ U.takeAnswers 1 $ U.run U.embedHandler vars defs $ goal

  -- 21689
testUnfoldEmbed' =
  putStrLn $ show $ U.takeAnswers 1 $ U.run U.embedHandler vars defs' $ goal

  -- ???
testUnfoldEmbedBackward =
  putStrLn $ show $ U.takeAnswers 1 $ U.run U.embedBackwardHandler vars defs $ goal

  -- ???
testUnfoldEmbedBackward' =
  putStrLn $ show $ U.takeAnswers 1 $ U.run U.embedBackwardHandler vars defs' $ goal

  -- 23179
testUnfoldEssentialHeight =
  putStrLn $ show $ U.takeAnswers 1 $ U.run (U.essentialHeightHandler esVars) vars defs $ goal

  -- 21689
testUnfoldEssentialHeight' =
  putStrLn $ show $ U.takeAnswers 1 $ U.run (U.essentialHeightHandler esVars) vars defs' $ goal
