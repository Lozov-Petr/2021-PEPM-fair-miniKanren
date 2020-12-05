module Tests.Reverso where

import Syntax

import FairEval
import FairStream
import Labels
import Embedding

import Program.List

import qualified Unfolding as U

----------------------------------------------------

def2 :: Def
def2 = Def "reverso" ["x", "y"] $
  (V "x" === nil &&& V "y" === nil) |||
  fresh ["e", "xs", "ys"] (
    V "x" === V "e" % V "xs" &&&
    call "appendo" [V "ys", V "e" % nil, V "y"] &&&
    call "reverso" [V "xs", V "ys"])

defs1 :: [Def]
defs1 = reverso

defs2 :: [Def]
defs2 = def2 : appendo

genList :: Int -> Tx
genList 0 = nil
genList n = C (show $ n - 1) [] % genList (n - 1)

goal1 :: Int -> G X
goal1 n = call "reverso" [genList n, V "answer"]

goal2 :: Int -> G X
goal2 n = call "reverso" [V "answer", genList n]

esVars = [("reverso", [0]), ("appendo", [0, 2])]

vars = ["answer"]

goal1Unit :: Int -> RunGoal X ()
goal1Unit = RG . goal1

goal2Unit :: Int -> RunGoal X ()
goal2Unit = RG . goal2

goal1Int :: Int -> RunGoal X Int
goal1Int  = RG . goal1

goal2Int :: Int -> RunGoal X Int
goal2Int  = RG . goal2

goal1Disj :: Int -> RunGoal X (Disj, Int)
goal1Disj = RG . goal1

goal2Disj :: Int -> RunGoal X (Disj, Int)
goal2Disj = RG . goal2

goal1Inv :: Int -> RunGoal X Invokes
goal1Inv  = RG . goal1

goal2Inv :: Int -> RunGoal X Invokes
goal2Inv  = RG . goal2

goal1Embed :: Int -> RunGoal X Streams
goal1Embed = RG . goal1

goal2Embed :: Int -> RunGoal X Streams
goal2Embed = RG . goal2

goal1InvEmbed :: Int -> RunGoal X StreamsDict
goal1InvEmbed = RG . goal1

goal2InvEmbed :: Int -> RunGoal X StreamsDict
goal2InvEmbed = RG . goal2

goal1Invs :: Int -> RunGoal X InvokesDict
goal1Invs = RG . goal1

goal2Invs :: Int -> RunGoal X InvokesDict
goal2Invs = RG . goal2

goal1Defs :: Int -> RunGoal X DefsLabel
goal1Defs = RG . goal1

goal2Defs :: Int -> RunGoal X DefsLabel
goal2Defs = RG . goal2

----------------------------------------------------
----------------------------------------------------
----------------------------------------------------

  -- first answer, length: 100
  -- step  :      40000
  -- path  :          1
  -- height:          1
  -- size  :          3
  -- disjs :          0
  -- conjs :          1
  -- actCnj:          1
  -- d in c:          0
  -- maxLs :          0
  -- swaps :          0

testUnit1_1 =
  putStrLn . show . run vars defs1 () . goal1Unit

----------------------------------------------------

  -- all answers, length: 100
  -- step  :    1400000
  -- path  :         74
  -- height:        126
  -- size  :        281
  -- disjs :         16
  -- conjs :        124
  -- actCnj:          1
  -- d in c:         16
  -- maxLs :          0
  -- swaps :          0

testUnit1_2 =
  putStrLn . show . takeAnswers 1 . run vars defs1 () . goal2Unit

----------------------------------------------------

  -- first answer, length: 100
  -- step  :    1620000
  -- path  :          4
  -- height:         15
  -- size  :         95
  -- disjs :         22
  -- conjs :         25
  -- actCnj:         14
  -- d in c:          1
  -- maxLs :          0
  -- swaps :          0

testUnit2_1 =
  putStrLn . show . takeAnswers 1 . run vars defs2 () . goal1Unit

----------------------------------------------------

  -- all answers, length: 100
  -- step  :      40000
  -- path  :          3
  -- height:          3
  -- size  :          7
  -- disjs :          1
  -- conjs :          2
  -- actCnj:          1
  -- d in c:          1
  -- maxLs :          0
  -- swaps :          0

testUnit2_2 =
  putStrLn . show . run vars defs2 () . goal2Unit

----------------------------------------------------

  -- first answer, length: 100
  -- step  :    4920000
  -- path  :          5
  -- height:         34
  -- size  :       3075
  -- disjs :        100
  -- conjs :       1437
  -- actCnj:         68
  -- d in c:          1
  -- maxLs :          0
  -- swaps :      72779

testInt1_1 =
  putStrLn . show . takeAnswers 1 . run vars defs1 (100 :: Int) . goal1Int

----------------------------------------------------
----------------------------------------------------

  -- all answers, length: 100
  -- step  :    2170000
  -- path  :          4
  -- height:         21
  -- size  :        239
  -- disjs :         13
  -- conjs :        106
  -- actCnj:          7
  -- d in c:          3
  -- maxLs :          0
  -- swaps :      31822

testInt1_2 =
  putStrLn . show . run vars defs1 (100 :: Int) . goal2Int

----------------------------------------------------

  -- first answer, length: 100
  -- step  :    4630000
  -- path  :          4
  -- height:         19
  -- size  :        833
  -- disjs :         83
  -- conjs :        333
  -- actCnj:         56
  -- d in c:          3
  -- maxLs :          0
  -- swaps :      59359

testInt2_1 =
  putStrLn . show . takeAnswers 1 . run vars defs2 (100 :: Int) . goal1Int

----------------------------------------------------

  -- all answers, length: 100
  -- step  :     140000
  -- path  :          3
  -- height:          4
  -- size  :         17
  -- disjs :          3
  -- conjs :          5
  -- actCnj:          2
  -- d in c:          1
  -- maxLs :          0
  -- swaps :       1911

testInt2_2 =
  putStrLn . show . run vars defs2 (100 :: Int) . goal2Int

----------------------------------------------------

  -- all answers, length: 100
  -- step  :    2770000
  -- path  :          4
  -- height:         66
  -- size  :       2457
  -- disjs :         44
  -- conjs :       1184
  -- actCnj:         26
  -- d in c:          1
  -- maxLs :          0
  -- swaps :       3822

testDisj1_1 =
  putStrLn . show . run vars defs1 (D 10, 10000 :: Int) . goal1Disj

----------------------------------------------------
----------------------------------------------------

  -- first answer, length: 100
  -- step  :    1560000
  -- path  :          4
  -- height:        103
  -- size  :       2415
  -- disjs :         28
  -- conjs :       1179
  -- actCnj:         16
  -- d in c:          1
  -- maxLs :          0
  -- swaps :       1061

testDisj1_2 =
  putStrLn . show . takeAnswers 1 . run vars defs1 (D 10, 10000 :: Int) . goal2Disj

----------------------------------------------------

  -- first answer, length: 100
  -- step  :    1620000
  -- path  :          4
  -- height:         15
  -- size  :         95
  -- disjs :         22
  -- conjs :         25
  -- actCnj:         14
  -- d in c:          1
  -- maxLs :          0
  -- swaps :          0

testDisj2_1 =
  putStrLn . show . takeAnswers 1 . run vars defs2 (D 10, 10000 :: Int) . goal1Disj


----------------------------------------------------

  -- all answers, length: 100
  -- step  :      40000
  -- path  :          3
  -- height:          3
  -- size  :          7
  -- disjs :          1
  -- conjs :          2
  -- actCnj:          1
  -- d in c:          1
  -- maxLs :          0
  -- swaps :          0

testDisj2_2 =
  putStrLn . show . run vars defs2 (D 10, 10000 :: Int) . goal2Disj

----------------------------------------------------
----------------------------------------------------

  -- first answer, length: 100
  -- step  :    1700000
  -- path  :          7
  -- height:         22
  -- size  :        303
  -- disjs :         24
  -- conjs :        127
  -- actCnj:         15
  -- d in c:          1
  -- maxLs :          0
  -- swaps :      71795

testInv1_1 =
  putStrLn . show . takeAnswers 1 . run vars defs1 (I 5) . goal1Inv

----------------------------------------------------

  -- all answers, length: 100
  -- step  :     110000
  -- path  :          3
  -- height:          7
  -- size  :         35
  -- disjs :          4
  -- conjs :         13
  -- actCnj:          3
  -- d in c:          1
  -- maxLs :          0
  -- swaps :       4393

testInv1_2 =
  putStrLn . show . run vars defs1 (I 5) . goal2Inv

----------------------------------------------------

  -- first answer, length: 100
  -- step  :    1690000
  -- path  :          6
  -- height:         18
  -- size  :        189
  -- disjs :         21
  -- conjs :         73
  -- actCnj:         13
  -- d in c:          1
  -- maxLs :          0
  -- swaps :      66508

testInv2_1 =
  putStrLn . show . takeAnswers 1 . run vars defs2 (I 5) . goal1Inv

----------------------------------------------------

  -- all answers, length: 100
  -- step  :      50000
  -- path  :          3
  -- height:          3
  -- size  :         11
  -- disjs :          2
  -- conjs :          3
  -- actCnj:          2
  -- d in c:          1
  -- maxLs :          0
  -- swaps :       2013

testInv2_2 =
  putStrLn . show . run vars defs2 (I 5) . goal1Inv

----------------------------------------------------
----------------------------------------------------

  -- did not wait for an answer
  -- step  :     360000
  -- path  :         16
  -- height:         69
  -- size  :       1287
  -- disjs :         16
  -- conjs :        627
  -- actCnj:         13
  -- d in c:          1
  -- maxLs :         39
  -- swaps :       1645

testInvLeftSubformulaCmpHeights1_1 =
  putStrLn . show . takeAnswers 1 . run vars defs1 (cmpSD cmpHeightsIgnoringLeftSubformula) . goal1InvEmbed

  ----------------------------------------------------

testInvLeftSubformulaCmpHeights1_2 =
  putStrLn . show . takeAnswers 1 . run vars defs1 (cmpSD cmpHeightsIgnoringLeftSubformula) . goal2InvEmbed

----------------------------------------------------

testInvLeftSubformulaCmpHeights2_1 =
  putStrLn . show . takeAnswers 1 . run vars defs2 (cmpSD cmpHeightsIgnoringLeftSubformula) . goal1InvEmbed

----------------------------------------------------

testInvLeftSubformulaCmpHeights2_2 =
  putStrLn . show . takeAnswers 1 . run vars defs2 (cmpSD cmpHeightsIgnoringLeftSubformula) . goal2InvEmbed

----------------------------------------------------
----------------------------------------------------

  -- all answers, length: 100
  -- step  :      40000
  -- path  :          1
  -- height:          1
  -- size  :          3
  -- disjs :          0
  -- conjs :          1
  -- actCnj:          1
  -- d in c:          0
  -- maxLs :       5024
  -- swaps :          0

testSubinvoke1_1_NonStritct =
  putStrLn . show . run vars defs1 NonStrict . goal1Invs

----------------------------------------------------

  -- all answers, length: 100
  -- step  :      40000
  -- path  :          3
  -- height:          3
  -- size  :         11
  -- disjs :          1
  -- conjs :          4
  -- actCnj:          1
  -- d in c:          1
  -- maxLs :          3
  -- swaps :         62

testSubinvoke1_2_NonStritct =
  putStrLn . show . run vars defs1 NonStrict . goal2Invs

----------------------------------------------------

  -- did not wait for all answers
  -- step  :   13720000
  -- path  :          5
  -- height:         17
  -- size  :        167
  -- disjs :         24
  -- conjs :         59
  -- actCnj:         15
  -- d in c:          1
  -- maxLs :        132
  -- swaps :      30686

  -- first answer, length: 100
  -- step  :    1660000
  -- path  :          5
  -- height:         18
  -- size  :        149
  -- disjs :         17
  -- conjs :         57
  -- actCnj:         12
  -- d in c:          1
  -- maxLs :         69
  -- swaps :       7162

testSubinvoke2_1_NonStritct =
  putStrLn . show . takeAnswers 1 . run vars defs2 NonStrict . goal1Invs

----------------------------------------------------

  -- all answers, length: 100
  -- step  :      40000
  -- path  :          3
  -- height:          3
  -- size  :          7
  -- disjs :          1
  -- conjs :          2
  -- actCnj:          1
  -- d in c:          1
  -- maxLs :         18
  -- swaps :          0

testSubinvoke2_2_NonStritct =
  putStrLn . show . run vars defs2 NonStrict . goal2Invs

----------------------------------------------------
----------------------------------------------------

testSubinvoke1_1_Stritct =
  putStrLn . show . run vars defs1 Strict . goal1Invs

----------------------------------------------------

testSubinvoke1_2_Stritct =
  putStrLn . show . run vars defs1 Strict . goal2Invs

----------------------------------------------------

testSubinvoke2_1_Stritct =
  putStrLn . show . run vars defs2 Strict . goal1Invs

----------------------------------------------------

testSubinvoke2_2_Stritct =
  putStrLn . show . run vars defs2 Strict . goal2Invs

----------------------------------------------------
----------------------------------------------------

  -- all answers, length: 100
  -- step  :      40000
  -- path  :          1
  -- height:          1
  -- size  :          3
  -- disjs :          0
  -- conjs :          1
  -- actCnj:          1
  -- d in c:          0
  -- maxLs :          0
  -- swaps :          0

testDefsApprox1_1 =
  putStrLn . show . run vars defs1 (toDA defs1) . goal1Defs

----------------------------------------------------

  -- all answers, length: 100
  -- step  :      50000
  -- path  :          1
  -- height:          2
  -- size  :          5
  -- disjs :          0
  -- conjs :          2
  -- actCnj:          1
  -- d in c:          0
  -- maxLs :          0
  -- swaps :        259

testDefsApprox1_2 =
  putStrLn . show . run vars defs1 (toDA defs1) . goal2Defs

----------------------------------------------------

  -- did not wait for all answers
  -- step  :   14750000
  -- path  :          4
  -- height:         17
  -- size  :        115
  -- disjs :         20
  -- conjs :         37
  -- actCnj:         14
  -- d in c:          1
  -- maxLs :          0
  -- swaps :      35487

  -- first answer, length: 100
  -- step  :    1590000
  -- path  :          3
  -- height:         14
  -- size  :        101
  -- disjs :         18
  -- conjs :         32
  -- actCnj:         14
  -- d in c:          1
  -- maxLs :          0
  -- swaps :       9996

testDefsApprox2_1 =
  putStrLn . show . takeAnswers 1 . run vars defs2 (toDA defs2) . goal1Defs

----------------------------------------------------

  -- all answers, length: 100
  -- step  :      40000
  -- path  :          3
  -- height:          3
  -- size  :          7
  -- disjs :          0
  -- conjs :          3
  -- actCnj:          1
  -- d in c:          0
  -- maxLs :          0
  -- swaps :        123

testDefsApprox2_2 =
  putStrLn . show . run vars defs2 (toDA defs2) . goal2Defs

----------------------------------------------------
----------------------------------------------------
----------------------------------------------------

  -- 5151
testUnfoldDefsRating1_1 =
  putStrLn . show . U.run100 (U.defsRatingSep defs1) vars defs1  . goal1

  -- 5250
testUnfoldDefsRating1_2 =
  putStrLn . show . U.takeAnswers 1 . U.run100 (U.defsRatingSep defs1) vars defs1  . goal2

  -- 5151
testUnfoldDefsRating2_1 =
  putStrLn . show . U.run100 (U.defsRatingSep defs2) vars defs2  . goal1

  -- 5252
testUnfoldDefsRating2_2 =
  putStrLn . show . U.run100 (U.defsRatingSep defs2) vars defs2  . goal2

  -- 5151
testUnfoldingFairConj1_1 =
  putStrLn . show . U.run100 (U.fairConj defs1 esVars) vars defs1 . goal1

  -- 5352
testUnfoldingFairConj1_2 =
  putStrLn . show . U.run100 (U.fairConj defs1 esVars) vars defs1 . goal2

  -- 5151
testUnfoldingFairConj2_1 =
  putStrLn . show . U.run100 (U.fairConj defs2 esVars) vars defs2 . goal1

  -- 5252
testUnfoldingFairConj2_2 =
  putStrLn . show . U.run100 (U.fairConj defs2 esVars) vars defs2 . goal2

testUnfoldEmbedConj1_1 =
  putStrLn . show . U.run U.embedHandler vars defs1 . goal1

testUnfoldEmbedConj1_2 =
  putStrLn . show . U.run U.embedHandler vars defs1 . goal2

testUnfoldEmbedConj2_1 =
  putStrLn . show . U.run U.embedHandler vars defs2 . goal1

testUnfoldEmbedConj2_2 =
  putStrLn . show . U.run U.embedHandler vars defs2 . goal2

testUnfoldEmbedBackwardConj1_1 =
  putStrLn . show . U.run U.embedBackwardHandler vars defs1 . goal1

testUnfoldEmbedBackwardConj1_2 =
  putStrLn . show . U.run U.embedBackwardHandler vars defs1 . goal2

testUnfoldEmbedBackwardConj2_1 =
  putStrLn . show . U.run U.embedBackwardHandler vars defs2 . goal1

testUnfoldEmbedBackwardConj2_2 =
  putStrLn . show . U.run U.embedBackwardHandler vars defs2 . goal2

  -- 5151
testUnfoldEssentialArgs1_1 =
  putStrLn . show . U.takeAnswers 1 . U.run100 (U.hasEssentialArgsSep esVars) vars defs1 . goal1

  -- 5252
testUnfoldEssentialArgs1_2 =
  putStrLn . show . U.takeAnswers 1 . U.run100 (U.hasEssentialArgsSep esVars) vars defs1 . goal2

  -- 5252
testUnfoldEssentialArgs2_2 =
  putStrLn . show . U.takeAnswers 1 . U.run100 (U.hasEssentialArgsSep esVars) vars defs2 . goal2

  -- 5151
testUnfoldEssentialArgs2_1 =
  putStrLn . show . U.takeAnswers 1 . U.run100 (U.hasEssentialArgsSep esVars) vars defs2 . goal1

testUnfoldEssentialHeightConj1_1 =
  putStrLn . show . U.run (U.essentialHeightHandler esVars) vars defs1 . goal1

testUnfoldEssentialHeightConj1_2 =
  putStrLn . show . U.run (U.essentialHeightHandler esVars) vars defs1 . goal2

testUnfoldEssentialHeightConj2_1 =
  putStrLn . show . U.run (U.essentialHeightHandler esVars) vars defs2 . goal1

testUnfoldEssentialHeightConj2_2 =
  putStrLn . show . U.run (U.essentialHeightHandler esVars) vars defs2 . goal2

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

-- 5151
testUnfoldSimplForwardOpt =
  putStrLn . show . U.takeAnswers 1 . U.run U.left2rightHandler vars defs1 . goal1

-- 181996
testUnfoldSimplForwardPes =
  putStrLn . show . U.takeAnswers 1 . U.run U.left2rightHandler vars defs2 . goal1

-- 5252
testUnfoldSimplBackwardOpt =
  putStrLn . show . U.takeAnswers 1 . U.run U.left2rightHandler vars defs2 . goal2

-- 176850
testUnfoldSimplBackwardPes =
  putStrLn . show . U.takeAnswers 1 . U.run U.left2rightHandler vars defs1 . goal2

----------------------------------------------------

testUnfoldNaiveForwardOpt m =
  putStrLn . show . U.takeAnswers 1 . U.run (U.naiveFairHandler m) vars defs1 . goal1

testUnfoldNaiveForwardPes m =
  putStrLn . show . U.takeAnswers 1 . U.run (U.naiveFairHandler m) vars defs2 . goal1

testUnfoldNaiveBackwardOpt m =
  putStrLn . show . U.takeAnswers 1 . U.run (U.naiveFairHandler m) vars defs2 . goal2

testUnfoldNaiveBackwardPes m =
  putStrLn . show . U.takeAnswers 1 . U.run (U.naiveFairHandler m) vars defs1 . goal2

----------------------------------------------------

testUnfoldFairWQOForwardOpt =
  putStrLn . show . U.takeAnswers 1 . U.run (U.essentialHeightHandler esVars) vars defs1 . goal1

testUnfoldFairWQOForwardPes =
  putStrLn . show . U.takeAnswers 1 . U.run (U.essentialHeightHandler esVars) vars defs2 . goal1

testUnfoldFairWQOBackwardOpt =
  putStrLn . show . U.takeAnswers 1 . U.run (U.essentialHeightHandler esVars) vars defs2 . goal2

testUnfoldFairWQOBackwardPes =
  putStrLn . show . U.takeAnswers 1 . U.run (U.essentialHeightHandler esVars) vars defs1 . goal2
