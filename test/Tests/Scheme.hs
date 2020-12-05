module Tests.Scheme where

import Syntax

import FairEval
import FairStream
import Labels
import Embedding
import Util

import Program.Scheme

import qualified Unfolding as U

---------------------------------------

defs :: [Def]
defs = fst $ tree2defs scheme

goal :: G X
goal = Invoke "eval" [V "quine", C "nil" [], C "val" [V "quine"]]

esVars = [("eval",           [0]), ("lookup",        [1]   ),
          ("lambda_handler", [] ), ("quote_handler", []    ),
          ("list_handler",   [] ), ("not_in_env",    [1]   ),
          ("map_eval_val",   [0]), ("eval_val",      []    ),
          ("eq_id",          [] ), ("eq_var",        [0, 1])
         ]

vars = ["quine"]

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
  -- step  :     360000
  -- path  :          9
  -- height:         37
  -- size  :      63971
  -- disjs :      17902
  -- conjs :      14083
  -- actCnj:        924
  -- d in c:        418
  -- maxLs :          0
  -- swaps :          0

testUnit =
  putStrLn $ show $ takeAnswers 1 $ run vars defs () goalUnit

----------------------------------------------------

  -- first answer
  -- step  :     960000
  -- path  :         20
  -- height:         26
  -- size  :     711263
  -- disjs :      82124
  -- conjs :     273507
  -- actCnj:      30204
  -- d in c:         23
  -- maxLs :          0
  -- swaps :      10267

testInt100 =
  putStrLn $ show $ takeAnswers 1 $ run vars defs (100 :: Int) goalInt

----------------------------------------------------

  -- first answer
  -- step  :     760000
  -- path  :          9
  -- height:         28
  -- size  :     415621
  -- disjs :      56611
  -- conjs :     151199
  -- actCnj:      21105
  -- d in c:         11
  -- maxLs :          0
  -- swaps :       2597

testDisj10 =
  putStrLn $ show $ takeAnswers 1 $ run vars defs (D 10, 10000 :: Int) goalDisj

----------------------------------------------------

  -- first answer
  -- step  :    3010000
  -- path  :         23
  -- height:         30
  -- size  :    3947961
  -- disjs :     202049
  -- conjs :    1771931
  -- actCnj:     166139
  -- d in c:          2
  -- maxLs :         18
  -- swaps :     454372

testShallowestIgnoringSubformula =
  putStrLn $ show $ takeAnswers 1 $ run vars defs (sc2 shallowestIgnoringSubformula eqAF) goalEmbed

----------------------------------------------------

  -- first answer
  -- step  :    1600000
  -- path  :         19
  -- height:         28
  -- size  :    1719163
  -- disjs :     122219
  -- conjs :     737362
  -- actCnj:      98101
  -- d in c:          2
  -- maxLs :         13
  -- swaps :     151162

testShallowestIgnoringLeftSubformula =
  putStrLn $ show $ takeAnswers 1 $ run vars defs (sc2 shallowestIgnoringLeftSubformula eqAF) goalEmbed

----------------------------------------------------

  -- first answer
  -- step  :    2480000
  -- path  :         20
  -- height:         27
  -- size  :    2026917
  -- disjs :     250059
  -- conjs :     763399
  -- actCnj:      49286
  -- d in c:       1584
  -- maxLs :       1450
  -- swaps :      34631

testInvLeftSubformula =
  putStrLn $ show $ takeAnswers 1 $ run vars defs (cmpSD shallowestIgnoringLeftSubformula) goalInvEmbed

----------------------------------------------------

  -- first answer
  -- step  :     460000
  -- path  :         19
  -- height:         26
  -- size  :     330525
  -- disjs :      47550
  -- conjs :     117712
  -- actCnj:       9547
  -- d in c:        814
  -- maxLs :        704
  -- swaps :       3859

testInvLeftSubformulaCmpHeights =
  putStrLn $ show $ takeAnswers 1 $ run vars defs (cmpSD cmpHeightsIgnoringLeftSubformula) goalInvEmbed

----------------------------------------------------

  -- first answer
  -- step  :     940000
  -- path  :         21
  -- height:         25
  -- size  :     748135
  -- disjs :      79469
  -- conjs :     294598
  -- actCnj:      37188
  -- d in c:         17
  -- maxLs :         23
  -- swaps :      31179

testInvsSubinvoke_NonStrict =
  putStrLn $ show $ takeAnswers 1 $ run vars defs NonStrict goalInvs

----------------------------------------------------

testInvsSubinvoke_Strict =
  putStrLn $ show $ takeAnswers 1 $ run vars defs Strict goalInvs

----------------------------------------------------

  -- first answer
  -- step  :     190000
  -- path  :         18
  -- height:         27
  -- size  :     100211
  -- disjs :      13543
  -- conjs :      36562
  -- actCnj:       4797
  -- d in c:         33
  -- maxLs :          0
  -- swaps :       6727

testDefsApprox =
  putStrLn $ show $ takeAnswers 1 $ run vars defs (toDA defs) goalDefs

----------------------------------------------------
----------------------------------------------------
----------------------------------------------------

  -- 62246
testUnfoldSimpl =
  putStrLn $ show $ U.takeAnswers 1 $ U.run U.left2rightHandler vars defs goal

  -- did not wait for an answer
testUnfoldDefsRating =
  putStrLn $ show $ U.takeAnswers 1 $ U.run100 (U.defsRatingSep defs) vars defs goal

  -- did not wait for an answer
testUnfoldFirstGoodCall =
  putStrLn $ show $ U.takeAnswers 1 $ U.run100 (U.firstGoodCallSep defs) vars defs goal

  -- 28070
testUnfoldEssentialArgs =
  putStrLn $ show $ U.takeAnswers 1 $ U.run100 (U.hasEssentialArgsSep esVars) vars defs goal

  -- 26022
testUnfoldingFairConj =
  putStrLn $ show $ U.takeAnswers 1 $ U.run100 (U.fairConj defs esVars) vars defs goal
