module Tests.Bottles where

import Syntax

import FairEval
import FairStream
import Labels
import Embedding
import Util

import Program.Bottles

import qualified Unfolding as U

----------------------------------------------------

defs :: [Def]
defs = bottles

defs1 :: [Def]
defs1 = bottles1

goal = Fresh "c" $
   Invoke "capacities1" [V "c"] :/\:
   Invoke "checkAnswer" [V "answer", V "c", int2nat 7, C "true" []]

esVars = [("capacities1",  []    ), ("checkAnswer",   []    ),
          ("checkAnswer'", [1]   ), ("isFinishState", []    ),
          ("checkStep",    []    ), ("|=|",           [0, 1]),
          ("get_capacity", []    ), ("doStep",        []    ),
          ("fst'",         []    ), ("snd'",          []    ),
          ("createState",  []    ), ("anotherBottle", []    ),
          ("add",          [0, 2]), ("greater",       [0, 1]),
          ("sub",          [1, 2])
         ]

vars = ["answer"]

goalUnit :: RunGoal X ()
goalUnit = RG goal

goalInt :: RunGoal X Int
goalInt = RG goal

goalInv :: RunGoal X Invokes
goalInv = RG goal

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
  -- step  :    2080000
  -- path  :         15
  -- height:         22
  -- size  :      96111
  -- disjs :      22250
  -- conjs :      25805
  -- actCnj:       6397
  -- d in c:          9
  -- maxLs :          0
  -- swaps :          0

testUnit =
  putStrLn $ show $ takeAnswers 1 $ run vars defs () goalUnit

----------------------------------------------------

  -- first answer
  -- step  :   23920000
  -- path  :         17
  -- height:         27
  -- size  :    2185409
  -- disjs :     290402
  -- conjs :     802302
  -- actCnj:     145327
  -- d in c:         15
  -- maxLs :          0
  -- swaps :     198131

testInt100 =
  putStrLn $ show $ takeAnswers 1 $ run vars defs (100 :: Int) goalInt

----------------------------------------------------

testInvoke50 =
  putStrLn $ show $ takeAnswers 1 $ run vars defs (I 50) goalInv

----------------------------------------------------

  -- first answer
  -- step  :    2080000
  -- path  :         15
  -- height:         22
  -- size  :      96111
  -- disjs :      22250
  -- conjs :      25805
  -- actCnj:       6397
  -- d in c:          9
  -- maxLs :          0
  -- swaps :          0

testDisj10 =
  putStrLn $ show $ takeAnswers 1 $ run vars defs (D 10, 10000 :: Int) goalDisj

----------------------------------------------------

testShallowestIgnoringEmbed =
  putStrLn $ show $ takeAnswers 1 $ run vars defs (sc2 shallowestIgnoringEmbed eqAF) goalEmbed

----------------------------------------------------

  -- did not wait for an answer
  -- step  :   19110000
  -- path  :         17
  -- height:         31
  -- size  :    2324083
  -- disjs :     306058
  -- conjs :     855983
  -- actCnj:      92056
  -- d in c:       1238
  -- maxLs :       6841
  -- swaps :     599778

testInvokeSubformula =
  putStrLn $ show $ takeAnswers 1 $ run vars defs (cmpSD shallowestIgnoringLeftSubformula) goalInvEmbed

----------------------------------------------------

  -- first answer
  -- step  :    4300000
  -- path  :         15
  -- height:         24
  -- size  :     319749
  -- disjs :      49650
  -- conjs :     110224
  -- actCnj:      23235
  -- d in c:          9
  -- maxLs :         22
  -- swaps :      32551

testInvsSubinvoke_NonStrict =
  putStrLn $ show $ takeAnswers 1 $ run vars defs NonStrict goalInvs

----------------------------------------------------

  -- first answer
  -- step  :    2080000
  -- path  :         15
  -- height:         22
  -- size  :      96111
  -- disjs :      22250
  -- conjs :      25805
  -- actCnj:       6397
  -- d in c:          9
  -- maxLs :          9
  -- swaps :          0

testInvsSubinvoke_Strict =
  putStrLn $ show $ takeAnswers 1 $ run vars defs Strict goalInvs

----------------------------------------------------

  -- first answer
  -- step  :    2080000
  -- path  :         15
  -- height:         22
  -- size  :      96111
  -- disjs :      22250
  -- conjs :      25805
  -- actCnj:       6397
  -- d in c:          9
  -- maxLs :          0
  -- swaps :          0

testDefsApprox =
  putStrLn $ show $ takeAnswers 1 $ run vars defs (toDA defs) goalDefs

----------------------------------------------------
----------------------------------------------------
----------------------------------------------------

  -- 237557
testUnfoldSimpl =
  putStrLn $ show $ U.takeAnswers 1 $ U.run U.left2rightHandler vars defs goal

testUnfoldSimplFair m =
  putStrLn $ show $ U.takeAnswers 1 $ U.run (U.naiveFairHandler m) vars defs goal

  -- did not wait for an answer
testUnfoldDefsRating =
  putStrLn $ show $ U.takeAnswers 1 $ U.run100 (U.defsRatingSep defs) vars defs goal

  -- 237557
testUnfoldFirstGoodCall =
  putStrLn $ show $ U.takeAnswers 1 $ U.run100 (U.firstGoodCallSep defs) vars defs goal

  -- 237557
testUnfoldEssentialArgs =
  putStrLn $ show $ U.takeAnswers 1 $ U.run100 (U.hasEssentialArgsSep esVars) vars defs goal

  -- 237557
testUnfoldingFairConj =
  putStrLn $ show $ U.takeAnswers 1 $ U.run100 (U.fairConj defs esVars) vars defs goal

testUnfoldSimplBad =
  putStrLn $ show $ U.takeAnswers 1 $ U.run100 U.left2rightHandler vars defs1 goal

testUnfoldSimplFairBad m =
  putStrLn $ show $ U.takeAnswers 1 $ U.run (U.naiveFairHandler m) vars defs1 goal


testUnfoldEssentialArgsBad =
  putStrLn $ show $ U.takeAnswers 1 $ U.run100 (U.hasEssentialArgsSep esVars) vars defs1 goal

  -- 237557
testUnfoldEmbed =
  putStrLn $ show $ U.takeAnswers 1 $ U.run U.embedHandler vars defs $ goal

  -- loooooong
testUnfoldEmbed1 =
  putStrLn $ show $ U.takeAnswers 1 $ U.run U.embedHandler vars defs1 $ goal

  --
testUnfoldEmbedBackward =
  putStrLn $ show $ U.takeAnswers 1 $ U.run U.embedBackwardHandler vars defs $ goal

  --
testUnfoldEmbedBackward1 =
  putStrLn $ show $ U.takeAnswers 1 $ U.run U.embedBackwardHandler vars defs1 $ goal

  -- 237557
testUnfoldEssentialHeight =
  putStrLn $ show $ U.takeAnswers 1 $ U.run (U.essentialHeightHandler esVars) vars defs $ goal

  -- 237557
testUnfoldEssentialHeight1 =
  putStrLn $ show $ U.takeAnswers 1 $ U.run (U.essentialHeightHandler esVars) vars defs1 $ goal
