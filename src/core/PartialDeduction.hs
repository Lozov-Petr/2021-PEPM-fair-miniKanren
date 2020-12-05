module PartialDeduction where

import qualified CPD.LocalControl   as LC
import           Data.Foldable      (foldlM)
import           Data.List          (find, intersect, partition, (\\))
import qualified Data.Map.Strict    as M
import           Data.Maybe         (mapMaybe)
import           Debug.Trace        (trace)
import           Driving            (generalizeGoals)
import           Embed
import qualified Eval               as E
import           Syntax
import           Text.Printf        (printf)
import           Util.Miscellaneous (fst3, show')

data PDTree = Fail
            | Success E.Sigma
            | Or [PDTree] (LC.Descend (G S)) E.Sigma
            | Conj [PDTree] [G S] E.Sigma
            | Gen PDTree (G S)
            | Leaf (G S) E.Sigma
            deriving (Show, Eq)

topLevel :: Program -> (PDTree, G S, [S])
topLevel (Program defs goal) =
    let gamma = E.updateDefsInGamma E.env0 defs in
    let (logicGoal, gamma', names) = E.preEval gamma goal in
    let nodes = [] in
    let descend = LC.Descend logicGoal [] in
    go descend gamma' nodes E.s0
  where
    go d@(LC.Descend goal ancs) env@(x, y, z) seen state =
      let treeResult =
            if any (isRenaming goal) seen
            then
              Leaf goal state
            else
              case find (`embed` goal) ancs of
                Just g ->
                  let ([newGoal], gen1, gen2, names) = generalizeGoals z [g] [goal] in
                  let env' = (x, y, names) in
                  let (ch, _, _) = go (LC.Descend newGoal ancs) env' seen state in
                  Gen ch newGoal
                Nothing ->
                  let (unfolded, gamma) = LC.oneStepUnfold goal env in
                  let normalized = LC.normalize unfolded in
                  let unified = mapMaybe (LC.unifyStuff state) normalized in
                  Or (map (\(g, s') ->
                        if null g
                        then
                          if null s'
                          then Fail
                          else Success s'
                        else
                          Conj (map (\h -> fst3 $ go (LC.Descend h (goal : ancs)) gamma (goal : seen) s') g) g s') unified)
                    d
                    state
      in (treeResult, V 1 === V 2, [4, 5, 6, 7])

oneStep :: G S -> E.Gamma -> E.Sigma -> ([([G S], E.Sigma)], E.Gamma)
oneStep goal env state =
    let (unfolded, gamma) = LC.oneStepUnfold goal env in
    let normalized = LC.normalize unfolded in
    let unified = mapMaybe (LC.unifyStuff state) normalized in
    (unified, gamma)

conjConcat :: [([([G S], E.Sigma)], E.Gamma)] -> Maybe ([([G S], E.Sigma)], E.Gamma)
conjConcat x = do
    let (goals, envs) = unzip x
    cGoals <- mapM goalsConcat goals
    cGammas <- gammaConcat envs
    return (cGoals, cGammas)
  where
    gammaConcat xs =
      foldlM (\(p, i, d) (_, i', d') -> do
                  i'' <- mergeI i i'
                  return (p, i'', mergeD d d'))
             (head xs)
             (tail xs)
    mergeD (x:xs) (y:ys) = if x >= y then x : xs else y : ys
    mergeI (d, f) i@(d', f') =
      let dd' = d \\ d' in
      let intersection = d `intersect` d' in
      if map f intersection /= map f' intersection
      then Nothing
      else Just $ foldl (\interp x -> E.extend interp x (f x)) i dd'

statesConcat :: [E.Sigma] -> Maybe E.Sigma
statesConcat = unifySubsts

goalsConcat :: [([a], E.Sigma)] -> Maybe ([a], E.Sigma)
goalsConcat x = do
  let (goals, states) = unzip x
  cStates <- statesConcat states
  return (reverse $ concat goals, cStates)

productList :: [[a]] -> [[a]]
productList []       = [[]]
productList (xs:xss) = [ h : t | h <- xs, t <- productList xss]

incrDeepOneStep :: Int -> Int -> G S -> E.Gamma -> E.Sigma -> Maybe ([([G S], E.Sigma)], E.Gamma)
incrDeepOneStep limit depth _ _ _ | limit < depth = Nothing
incrDeepOneStep limit depth goal env state =
    let (unified, gamma) = oneStep goal env state in
    if all (not . null . fst) unified
    then do
      (goals, newEnv) <-
        foldlM
          (\(acc, env) (gs, sigma) -> do
            (x, env') <-
              foldlM
                (\(acc, env) g -> do
                    (x, env') <- incrDeepOneStep limit (depth + 1) g env sigma
                    return (x : acc, env')
                )
                ([], env)
                gs
            return (acc ++ x, env')
          )
          ([], gamma)
          unified
      return (mapMaybe goalsConcat $ productList goals, newEnv)
    else
      return (unified, gamma)

realIncrDeep :: Int -> (a -> a) -> a -> a
realIncrDeep 0 _ x = x
realIncrDeep globalLimit f x =
    go 1 1 f (f x)
  where
    go _ curr f x | globalLimit <= curr = x
    go localLimit curr f x | localLimit <= curr =
      go (max (localLimit * 2) globalLimit) (curr + 1) f (f x)
    go localLimit curr f x =
      go localLimit (curr + 1) f (f x)

leaf :: E.Sigma -> PDTree
leaf [] = Fail
leaf s  = Success s

unifySubsts :: [E.Sigma] -> Maybe E.Sigma
unifySubsts [] = return []
unifySubsts [s] = return s
unifySubsts (x : y : xs) = do
    s <- go x y
    unifySubsts (s : xs)
  where
    go s [] = Just s
    go s ((v, t) : rest) = do
      s' <- E.unify (Just s) (V v) t
      go s' rest

conjToList :: G a -> [G a]
conjToList (g :/\: h) = conjToList g ++ conjToList h
conjToList x          = [x]

conj :: [G a] -> G a
conj (a:as) = foldl (:/\:) a as

nonConjunctive :: Program -> (PDTree, G S, [S])
nonConjunctive (Program defs goal) =
    let gamma = E.updateDefsInGamma E.env0 defs in
    let (logicGoal, gamma', names) = E.preEval gamma goal in
    let nodes = [] in
    let descend = LC.Descend logicGoal [] in
    go descend gamma' nodes E.s0
  where
    go d@(LC.Descend goal' ancs) env@(x, y, z) seen state =
     trace (printf "\nGo\nGoal:\n%s\n\nSeen\n%s\n\nAncs:\n%s\n" (show goal') (show' seen) (show' ancs)) $
    --  if head z > 45
    --  then (Leaf (V 1 === V 13) [], undefined, undefined )
    --  else
      let goal = conj $ E.substitute state (conjToList goal') in
      let treeResult =
            if any (\x -> isVariant (conjToList x) (conjToList goal)) seen
            then
              Leaf goal state
            else
              case find (\x -> conjToList x `embed` conjToList goal) ancs of
                Just g ->
                  let (newGoals, gen1, gen2, names) = generalizeGoals z (conjToList g) (conjToList goal) in
                  let newGoal = conj newGoals in
                  if newGoals `isRenaming` conjToList goal
                  then
                    Leaf (V 1 === V 2) []
                  else
                    let env' = (x, y, names) in
                    let (ch, _, _) = go (LC.Descend newGoal ancs) env' seen state in
                    Gen ch newGoal
                Nothing ->

                  let incr = incrDeepOneStep 2 0 goal env state in
                  trace "=======================================" $
                  (case incr of
                    Nothing -> trace "Nothing"
                    Just incr' -> trace (printf "\nIncrDeepOneStep\nGoal:\n%s\nResult:\n%s\n\n" (show goal) (show' $ fst incr'))) $
                  trace "=======================================" $

                  let (unified, gamma) = oneStep goal env state in
                  Or (map (\(g, s') ->
                        if null g
                        then
                          leaf s'
                        else
                          let (newEnv, onceUnfolded) =
                                foldl (\(env, acc) g' ->
                                        let (x, env') = oneStep g' env s' in


                                        let incr = incrDeepOneStep 2 0 g' env s' in
                                        trace "=======================================" $
                                        (case incr of
                                          Nothing -> trace "Nothing"
                                          Just incr' -> trace (printf "\nIncrDeepOneStep\nGoal:\n%s\nResult:\n%s\n\n" (show g') (show' $ fst incr'))) $
                                        trace "=======================================" 

                                        (env', x : acc)
                                      ) (gamma, []) g in

                          let sequenced = sequence $ reverse onceUnfolded in

                          if any checkConflicts $ map (map snd) sequenced
                          then
                            let filtered = filter (not . checkConflicts . map snd) sequenced in
                            let ch =
                                  map (\seq ->
                                            let (gs, substs) = unzip seq in
                                            let conjunction = concat gs in
                                            let maybeSubst = unifySubsts substs in
                                            case maybeSubst of
                                              Nothing -> Fail
                                              Just newSubst ->
                                                let ch =
                                                      if null conjunction
                                                      then leaf newSubst
                                                      else
                                                        fst3 $ go (LC.Descend (conj conjunction) (conj g : goal : ancs))
                                                                  newEnv
                                                                  (conj g : goal : seen)
                                                                  newSubst
                                                in
                                                Conj [ch] (concatMap fst seq) newSubst
                                      )
                                      filtered in
                            Or ch (LC.Descend (conj g) (goal : ancs)) s'
                          else
                            let ch = map (\h -> fst3 $ go (LC.Descend h (goal : ancs)) gamma (goal : seen) s') g in
                            Conj ch g s')
                          unified)
                    d
                    state
      in (treeResult, V 1 === V 2, [4, 5, 6, 7])

checkConflicts :: [E.Sigma] -> Bool
checkConflicts sigmas =
    let conflicting = findConflicting sigmas in
    any (\x -> length x /= 1) conflicting

collectSubsts :: PDTree -> [E.Sigma]
collectSubsts (Or ch _ _) =
    mapMaybe go ch
  where
    go Fail         = Nothing
    go (Success s)  = Just s
    go (Or _ _ s)   = Just s
    go (Conj _ _ s) = Just s
    go (Gen _ _)    = Nothing
    go (Leaf _ s)   = Just s
collectSubsts (Leaf _ s) = [s]
collectSubsts x = trace (printf "\nPattern matching Failed on\n%s\n" $ show x) []

isConflicting :: E.Sigma -> E.Sigma -> Bool
isConflicting s1 s2 =
    -- trace (printf "isConflicting\nS1: %s\nS2: %s\n"  (show s1) (show s2)) $
    let m1 = M.fromList s1 in
    let m2 = M.fromList s2 in
    let intersection = M.intersectionWith (\x y -> (E.walk x s1, E.walk y s2)) m1 m2 in
    M.size intersection > 0 &&
    any (uncurry conflicting) intersection
  where
    conflicting (C x xs) (C y ys) = x /= y || length xs /= length ys || any (uncurry conflicting) (zip xs ys)
    conflicting _ _ = False

findConflicting :: [E.Sigma] -> [[E.Sigma]]
findConflicting [] = []
findConflicting [x] = [[x]]
findConflicting (x:xs) =
    -- trace (printf "findConflicting\n%s\n" (show (x:xs))) $
    go [x] [] xs
  where
    go [] [] [] = []
    go [] conf [] = [reverse conf]
    go [] conf unConf = reverse conf : findConflicting unConf
    go (x:xs) conf unConf =
      -- trace "find Conflictnig  : go " $
      let (conf', unConf') = partition (isConflicting x) unConf in
      go (xs ++ conf') (x : conf) unConf'
