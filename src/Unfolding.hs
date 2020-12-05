module Unfolding where

import qualified Data.Map.Strict as M

import Text.Printf  (printf)
import Data.Maybe   (mapMaybe)

import Syntax

import Eval         (sEmpty, walk)
import FairEval     (Answers((:::), Nil), toSemG, takeAns, def2fun, prepareAnswer)
import FairStream   (Fun, Subst, Er)
import Util         (unify, substInT, termHeight)
import DefsAnalysis (Approx, def2approx)

import Embedding    (embedT)

----------------------------------------------------

type Call     = (Name, [Ts])

type Conj a   = (Call, a)

data Stream a = Disj (Stream a) (Stream a)
              | Conj Subst [Conj a]

type Separator a = Subst -> [Conj a] -> [Int]
type Predicate a = Subst -> Conj a -> Bool
type Updater   a = Subst -> Conj a -> Call -> a
type New       a = Subst -> Call -> Maybe a -> a

type ConjHandler a = (Separator a, Updater a, New a)

----------------------------------------------------

instance Show a => Show (Stream a) where
  show (Disj a b) = printf "%s\n\n%s" (show a) $ show b
  show (Conj s c) = foldr (\((n,a), b) acc -> printf "%s(%s) {%s}\n%s" n (showArgs s a) (show b) acc) "" c where
    showArgs s []     = ""
    showArgs s [a]    = show $ substInT s a
    showArgs s (a:as) = printf "%s, %s" (show $ substInT s a) $ showArgs s as

----------------------------------------------------

step :: ConjHandler a -> [Fun] -> Stream a -> Er (Maybe (Stream a), [Subst])
step h fs (Disj a b) =
  case step h fs a of
    Left e             -> Left e
    Right (Nothing, x) -> Right (Just b         , x)
    Right (Just c , x) -> Right (Just $ Disj b c, x)
step h@(sep, upd, new) fs (Conj s cs) =
  case sep s cs of
    []  -> step h fs (Conj s $ map (\(c, m) -> (c, new s c $ Just m)) cs)
    i:_ ->
      let (cs1, c : cs2) = splitAt i cs in
        case unfold upd fs s c of
          Left e                                -> Left e
          Right Nothing                         -> Right (Nothing, [])
          Right (Just s) | null cs1 && null cs2 -> Right $ splitAnswers s
          Right (Just s)                        -> Right (Just $ attachConjs cs1 cs2 s, [])

splitAnswers :: Stream a -> (Maybe (Stream a), [Subst])
splitAnswers (Disj a b) =
  case (splitAnswers a, splitAnswers b) of
    ((Nothing, a1), (Nothing, a2)) -> (Nothing          , a1 ++ a2)
    ((Just s1, a1), (Nothing, a2)) -> (Just s1          , a1 ++ a2)
    ((Nothing, a1), (Just s2, a2)) -> (Just s2          , a1 ++ a2)
    ((Just s1, a1), (Just s2, a2)) -> (Just $ Disj s1 s2, a1 ++ a2)
splitAnswers (Conj a []) = (Nothing, [a])
splitAnswers s           = (Just s, [])

attachConjs :: [Conj a] -> [Conj a] -> Stream a -> Stream a
attachConjs cs1 cs2 (Disj a b)  = Disj (attachConjs cs1 cs2 a) $ attachConjs cs1 cs2 b
attachConjs cs1 cs2 (Conj s cs) = Conj s $ cs1 ++ cs ++ cs2

unfold :: Updater a -> [Fun] -> Subst -> Conj a -> Er (Maybe (Stream a))
unfold upd fs subst@(i, s) ((n, a), m) =
  case lookup n fs of
    Nothing -> Left $ printf "Undefined relation '%s'." n
    Just (x, g) | length x == length a ->
      case toSemG (zip x a) i g of
        Left e       -> Left e
        Right (g, i) -> Right $ goalToStream (\s c -> upd s substedConj c) (i, s) g
    Just (x, _) -> Left $ printf "Unexpected count of arguments (relation: '%s', expected: %d, actual: %d)" n (length x) $ length a
  where
    substedConj = ((n, map (substInT subst) a), m)

goalToStream :: (Subst -> Call -> a) -> Subst -> G S -> Maybe (Stream a)
goalToStream upd s g = g2s (Conj s []) g where
  disjCmb :: Maybe (Stream a) -> Maybe (Stream a) -> Maybe (Stream a)
  disjCmb Nothing  b        = b
  disjCmb a        Nothing  = a
  disjCmb (Just a) (Just b) = Just $ Disj a b
  g2s (Disj a b)  g            = disjCmb (g2s a g) $ g2s b g
  g2s (Conj s cs) (t1 :=: t2)  = unify s t1 t2 >>= \s -> return $ Conj s cs
  g2s (Conj s cs) (Invoke n a) = return $ Conj s $ cs ++ [((n, a), upd s (n, a))]
  g2s s           (g1 :/\: g2) = g2s s g1 >>= \s -> g2s s g2
  g2s s           (g1 :\/: g2) = disjCmb (g2s s g1) $ g2s s g2

----------------------------------------------------

type RunAnswers =  Answers ([(X, Ts)], Int) (Maybe String, Int)

takeAnswers :: Int -> RunAnswers -> RunAnswers
takeAnswers = takeAns 0

prepareStream :: [X] -> New a -> G X -> Er (Maybe (Stream a))
prepareStream vars new g =
  case toSemG (zip vars $ map V [0..]) (length vars) g of
    Left e       -> Left $ printf "Error in initial goal. %s" e
    Right (g, i) -> Right $ goalToStream (\s c -> new s c Nothing) (i, sEmpty) g

run :: ConjHandler a -> [X] -> [Def] -> G X -> RunAnswers
run h@(_,_,new) vars defs g =
  case prepareStream vars new g of
    Left e         -> Nil (Just e, 0)
    Right Nothing  -> Nil (Nothing, 0)
    Right (Just s) -> run' h 1 s
  where
    funs :: [Fun]
    funs = map def2fun defs
    run' :: ConjHandler a -> Int -> Stream a -> RunAnswers
    run' h i s =
      case step h funs s of
        Left e             -> Nil (Just e, i)
        Right (Nothing, a) -> foldr (\s acc -> (prepareAnswer s vars, i) ::: acc) (Nil (Nothing, i)) a
        Right (Just s , a) -> foldr (\s acc -> (prepareAnswer s vars, i) ::: acc) (run' h (i+1) s) a

run100 :: ConjHandler a -> [X] -> [Def] -> G X -> RunAnswers
run100 h = run $ conjHandlerSeqOr h $ naiveFairHandler 100

----------------------------------------------------

sepByPred :: Predicate a -> Separator a
sepByPred pred s = map fst . filter (pred s . snd) . zip [0..]


updUnion :: Updater a -> Updater b -> Updater (a, b)
updUnion upd1 upd2 s (prevCall, (a, b)) call = (upd1 s (prevCall, a) call, upd2 s (prevCall, b) call)

newUnion :: New a -> New b -> New (a, b)
newUnion new1 new2 s c Nothing      = (new1 s c Nothing,  new2 s c Nothing)
newUnion new1 new2 s c (Just (a,b)) = (new1 s c $ Just a, new2 s c $ Just b)

conjHandlerSeqOr :: ConjHandler a -> ConjHandler b -> ConjHandler (a, b)
conjHandlerSeqOr (sep1, upd1, new1) (sep2, upd2, new2) = (sep, updUnion upd1 upd2, newUnion new1 new2) where
  sep s cs = case sep1 s $ map (\(c,(a,_)) -> (c,a)) cs of
               [] -> sep2 s $ map (\(c,(_,b)) -> (c,b)) cs
               l  -> l

conjHandlerOr :: ConjHandler a -> ConjHandler b -> ConjHandler (a, b)
conjHandlerOr (sep1, upd1, new1) (sep2, upd2, new2) = (sep, updUnion upd1 upd2, newUnion new1 new2) where
  sep s cs = disjLists (sep1 s $ map (\(c,(a,_)) -> (c,a)) cs) $ sep2 s $ map (\(c,(_,b)) -> (c,b)) cs
  disjLists []        b                  = b
  conjLists a         []                 = a
  conjLists l1@(x:xs) l2@(y:ys) | x == y = x : conjLists xs ys
                               | x < y  = x : conjLists xs l2
                               | x > y  = y : conjLists l1 ys

conjHandlerAnd :: ConjHandler a -> ConjHandler b -> ConjHandler (a, b)
conjHandlerAnd (sep1, upd1, new1) (sep2, upd2, new2) = (sep, updUnion upd1 upd2, newUnion new1 new2) where
  sep s cs = conjLists (sep1 s $ map (\(c,(a,_)) -> (c,a)) cs) $ sep2 s $ map (\(c,(_,b)) -> (c,b)) cs
  conjLists []        _                  = []
  conjLists _         []                 = []
  conjLists l1@(x:xs) l2@(y:ys) | x == y = x : conjLists xs ys
                                | x < y  = conjLists xs l2
                                | x > y  = conjLists l1 ys

unitUpdater :: Updater ()
unitUpdater _ _ _ = ()

unitNew :: New ()
unitNew _ _ _ = ()

unitFairHandler :: Separator () -> ConjHandler ()
unitFairHandler s = (s, unitUpdater, unitNew)

left2rightHandler :: ConjHandler ()
left2rightHandler = unitFairHandler $ \_ _ -> [0]

naiveFairHandler :: Int -> ConjHandler Int
naiveFairHandler n = (sepByPred pred, upd, new) where
  pred _ (_, i) = i > 0
  upd _ (_, 0) _ = 0
  upd _ (_, m) _ = m - 1
  new _ _ _ = n

defsRatingSep :: [Def] -> ConjHandler ()
defsRatingSep ds = unitFairHandler sep where
  sep s cs = [indexOfMin 0 0 1 $ map (getIndex s) cs]
  aps = map def2approx ds
  indexOfMin :: Int -> Int -> Double -> [Double] -> Int
  indexOfMin _ j _ []             = j
  indexOfMin i j a (x:xs) | x < a = indexOfMin (i+1) i x xs
  indexOfMin i j a (x:xs)         = indexOfMin (i+1) j a xs
  getIndex :: Subst -> Conj a -> Double
  getIndex s ((n, a), _) =
    case lookup n aps of
      Nothing    -> error $ printf "Undefined relation:'%s'." n
      Just cases ->
        let l1 = length cases in
        let l2 = length $ mapMaybe (\c -> foldl (\acc (t,u) -> acc >>= \s -> unify s t u) (Just s) $ zip a c) cases in
        fromIntegral l2 / fromIntegral l1

andForPreds :: Predicate a -> Predicate a -> Predicate a
andForPreds p1 p2 s c = p1 s c && p2 s c

isGoodCall :: [Approx] -> Predicate ()
isGoodCall aps s ((n, a), _) =
  case lookup n aps of
    Nothing    -> error $ printf "Undefined relation:'%s'." n
    Just cases ->
      let l1 = length cases in
      let l2 = length $ mapMaybe (\c -> foldl (\acc (t,u) -> acc >>= \s -> unify s t u) (Just s) $ zip a c) cases in
      l1 <= 1 || l1 > l2

hasEssentialArgs :: [(Name, [Int])] -> Predicate ()
hasEssentialArgs eArgs s ((n, a), _) =
  case lookup n eArgs of
    Nothing   -> error $ printf "Undefined relation:'%s'." n
    Just args -> null args || any (isNotFree . (a !!)) args where
      isNotFree :: Ts -> Bool
      isNotFree t =
        case walk t $ snd s of
          V _ -> False
          _   -> True

firstGoodCallSep :: [Def] -> ConjHandler ()
firstGoodCallSep = unitFairHandler . sepByPred . isGoodCall . map def2approx

hasEssentialArgsSep :: [(Name, [Int])] -> ConjHandler ()
hasEssentialArgsSep = unitFairHandler . sepByPred . hasEssentialArgs

fairConj :: [Def] -> [(Name, [Int])] -> ConjHandler ()
fairConj ds as = unitFairHandler . sepByPred $ andForPreds (isGoodCall $ map def2approx ds) $ hasEssentialArgs as

embedCall :: Call -> Call -> Bool
embedCall (n, a1) (m, a2) =
 -- trace (show (n, a1) ++ " || " ++ show (m, a2)) $
  if n == m && length a1 == length a2
  then all (uncurry embedT) $ zip a1 a2
  else False

embedHandler :: ConjHandler [Call]
embedHandler = (sepByPred pred, upd, new) where
  new _ _ _ = []
  upd _ (call, p) _ = call : p
  pred s ((n, a), p) = not $ any (flip embedCall (n, map (substInT s) a)) p

embedBackwardHandler :: ConjHandler [Call]
embedBackwardHandler = (sepByPred pred, upd, new) where
  new _ _ _ = []
  upd s (call, p) _ = call : p
  pred s ((n, a), p) = not $ any (embedCall (n, map (substInT s) a)) p


essentialHeightHandler :: [(Name, [Int])] -> ConjHandler (M.Map Name [Int], Int)
essentialHeightHandler eArgs = conjHandlerSeqOr (sepByPred pred, upd, new) (naiveFairHandler 1) where
  new _ _ _ = M.empty
  upd s ((n, a), p) _ =
    case lookup n eArgs of
      Nothing   -> error $ printf "Undefined relation: '%s'." n
      Just args -> M.insert n (map (termHeight s . snd) $ filter (flip elem args . fst) $ zip [0..] a) p
  pred s ((n, a), p) =
    case M.lookup n p of
      Nothing -> True
      Just [] -> True
      Just prevH ->
        case lookup n eArgs of
          Nothing   -> error $ printf "Undefined relation: '%s'." n
          Just args ->
            let sArgs = map snd $ filter (flip elem args . fst) $ zip [0..] a in
            let h     = map (termHeight s) sArgs in
            any (isNotFree s) sArgs &&
            all (\(a,b) -> a >= b) (zip prevH h) &&
            any (\(a,b) -> a > b) (zip prevH h)
  isNotFree s t =
    case walk t $ snd s of
      V _ -> False
      _   -> True
