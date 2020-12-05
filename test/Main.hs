module Main where

import qualified Tests.Bottles     as Bottles
import qualified Tests.Bridge      as Bridge
import qualified Tests.Hanoi       as Hanoi
import qualified Tests.Reverso     as Reverso
import qualified Tests.Sorto       as Sorto

import System.Environment

doTest par = case par of
  -- line 1
  "ReversoBiasedForwardOpt30"  -> Reverso.testUnfoldSimplForwardOpt 30
  "ReversoBiasedForwardPes30"  -> Reverso.testUnfoldSimplForwardPes 30
  "ReversoNaiveForwardOpt30"   -> Reverso.testUnfoldNaiveForwardOpt 10000 30
  "ReversoNaiveForwardPes30"   -> Reverso.testUnfoldNaiveForwardPes 11    30
  "ReversoFairWQOForwardOpt30" -> Reverso.testUnfoldFairWQOForwardOpt 30
  "ReversoFairWQOForwardPes30" -> Reverso.testUnfoldFairWQOForwardPes 30

  -- line 2
  "ReversoBiasedForwardOpt60"  -> Reverso.testUnfoldSimplForwardOpt 60
  "ReversoBiasedForwardPes60"  -> Reverso.testUnfoldSimplForwardPes 60
  "ReversoNaiveForwardOpt60"   -> Reverso.testUnfoldNaiveForwardOpt 10000 60
  "ReversoNaiveForwardPes60"   -> Reverso.testUnfoldNaiveForwardPes 11    60
  "ReversoFairWQOForwardOpt60" -> Reverso.testUnfoldFairWQOForwardOpt 60
  "ReversoFairWQOForwardPes60" -> Reverso.testUnfoldFairWQOForwardPes 60

  -- line 3
  "ReversoBiasedForwardOpt90"  -> Reverso.testUnfoldSimplForwardOpt 90
  "ReversoBiasedForwardPes90"  -> Reverso.testUnfoldSimplForwardPes 90
  "ReversoNaiveForwardOpt90"   -> Reverso.testUnfoldNaiveForwardOpt 10000 90
  "ReversoNaiveForwardPes90"   -> Reverso.testUnfoldNaiveForwardPes 12    90
  "ReversoFairWQOForwardOpt90" -> Reverso.testUnfoldFairWQOForwardOpt 90
  "ReversoFairWQOForwardPes90" -> Reverso.testUnfoldFairWQOForwardPes 90


main = do
  args <- getArgs
  case args of
    [par] -> do
      doTest par
    _ -> putStrLn "Wrong number of arguments"
