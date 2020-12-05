module Tests.BadExamples where

import Syntax
import Program.List

import qualified Tests.Sorto as S
import qualified Unfolding   as U

repeato :: Def
repeato = Def "repeato" ["x", "y"] $
  (V "x" === V "y") |||
  fresh ["z"] (
    (call "appendo" [V "x", V "z", V "y"]) &&&
    (call "repeato" [V "x", V "z"]))

defs1 :: [Def]
defs1 = repeato : appendoDef : S.defs1

defs2 :: [Def]
defs2 = repeato : appendoDef : S.defs2

genList :: Int -> Tx
genList n = gen 1 where
  gen i | i == n = S.genNat n % nil
        | True   = S.genNat i % gen (i + 1)

goal :: Int -> G X
goal n =
  let vs = map (('a':) . show) [1..n] in
  fresh vs $
  (V "list" === foldr (%) nil (map V vs)) &&&
  call "repeato" [S.genList n, V "list"] &&&
  call "sorto" [V "list", genList n]

esVars = [("appendo", [0, 2]), ("repeato",   []   ),
          ("sorto",   [1]   ), ("smallesto", [0, 2]),
          ("minmaxo", []    ), ("leo",       [0, 1]),
          ("gto",     [0, 1])
         ]

vars = ["list"]


  -- 5 -> 547
testUnfoldSimpl1 =
  putStrLn . show . U.run U.left2rightHandler vars defs1 . goal

  -- 5 -> 120
testUnfoldSimpl2 =
  putStrLn . show . U.run U.left2rightHandler vars defs2 . goal

  -- 5 -> 70
testUnfoldDefsRating1 =
  putStrLn . show . U.run100 (U.defsRatingSep defs1) vars defs1 . goal

  -- 5 -> 70
testUnfoldDefsRating2 =
  putStrLn . show . U.run100 (U.defsRatingSep defs2) vars defs2 . goal

  -- 5 ->
testUnfoldFirstGoodCall1 =
  putStrLn . show . U.run100 (U.firstGoodCallSep defs1) vars defs1 . goal

  -- 5 ->
testUnfoldFirstGoodCall2 =
  putStrLn . show . U.run100 (U.firstGoodCallSep defs2) vars defs2 . goal

  -- 5 ->
testUnfoldEssentialArgs1 =
  putStrLn . show . U.run100 (U.hasEssentialArgsSep esVars) vars defs1 . goal

  -- 5 ->
testUnfoldEssentialArgs2 =
  putStrLn . show . U.run100 (U.hasEssentialArgsSep esVars) vars defs2 . goal

  -- 5 ->
testUnfoldingFairConj1 =
  putStrLn . show . U.run100 (U.fairConj defs1 esVars) vars defs1 . goal

  -- 5 ->
testUnfoldingFairConj2 =
  putStrLn . show . U.run100 (U.fairConj defs2 esVars) vars defs2 . goal
