module FairEval where

import Text.Printf
import Debug.Trace

import Syntax
import qualified Eval

import FairStream
import Util

---------------------------------------

toSemTs :: [(X, Ts)] -> [Tx] -> Er [Ts]
toSemTs e l = foldr combine (return []) l where
  combine x l = toSemT e x >>= \x -> l >>= \l -> return $ x : l

toSemT :: [(X, Ts)] -> Tx -> Er Ts
toSemT e (V x) = case lookup x e of
    Nothing -> Left $ printf "Unexpected variable '%s'." x
    Just x  -> return x
toSemT e (C n a) = toSemTs e a >>= return . (C n)

toSemG :: [(X, Ts)] -> Int -> G X -> Er (G S, Int)
toSemG e i (t1 :=: t2)  = do t1' <- toSemT e t1
                             t2' <- toSemT e t2
                             return (t1' :=: t2', i)
toSemG e i (g1 :/\: g2) = do (g1', j) <- toSemG e i g1
                             (g2', k) <- toSemG e j g2
                             return (g1' :/\: g2', k)
toSemG e i (g1 :\/: g2) = do (g1', j) <- toSemG e i g1
                             (g2', k) <- toSemG e j g2
                             return (g1' :\/: g2', k)
toSemG e i (Invoke n a) = do a' <- toSemTs e a
                             return (Invoke n a', i)
toSemG e i (Fresh n g)  = toSemG ((n, V i) : e) (i + 1) g
toSemG e i _            = Left "Let-expressions aren't supported."

---------------------------------------

fillHole :: HoleStream l -> a -> Er (GenStream a l)
fillHole (Goal g _  ) a = return $ Goal g a
fillHole (Conj s l g) a = fillHole s a >>= \s -> return $ Conj s l g
fillHole _            _ = Left "Unexpected disjunction in stream with hole."


swapConjs :: Labels l p => p -> Stream l -> HoleStream l -> Er (Stream l)
swapConjs p = swap p id where
  swap :: Labels l p => p -> (HoleStream l -> HoleStream l) -> Stream l -> HoleStream l -> Er (Stream l)
  swap par conjs (Goal g s)   conj = fillHole conj s >>= \s -> return $ Conj s (new par s) $ conjs (Goal g ())
  swap par conjs (Disj p q)   conj = swap par conjs p conj >>= \p -> swap par conjs q conj >>= \q -> return $ Disj p q
  swap par conjs (Conj s l g) conj = swap par (\s -> conjs $ Conj s (keep par l) g) s conj

---------------------------------------

eval :: Labels l p => p -> [Fun] -> Stream l -> Er (Maybe (Stream l), Maybe Subst, Logs)
eval par fs (Goal (p :=: q) s)        = return (Nothing, unify s p q, [Unify])
eval par fs (Goal (p :\/: q) s)       = return (Just $ Disj (Goal p s) $ Goal q s, Nothing, [DisjIntro])
eval par fs (Goal (p :/\: g) s)       = let p' = Goal p s in
                                        return (Just $ Conj p' (new par p') $ Goal g (), Nothing, [ConjIntro])
eval _   _  (Goal (Fresh _ _) _)      = Left "Unexpected fresh-expresseion."
eval _   _  (Goal (Let _ _) _)        = Left "Let-expressions aren't supported."
eval par fs (Goal (Invoke n a) (i,s)) =
  case lookup n fs of
    Nothing      -> Left $ printf "Call of undefined relation '%s'." n
    Just (ns, g) ->
      if length ns == length a
        then case toSemG (zip ns a) i g of
               Left msg -> Left $ printf "Error invoke '%s'. %s" n msg
               Right (g,j) -> return $ (Just $ Goal g (j,s), Nothing, [InvokeStep n])
        else Left $ printf
               "Unexpected count of relation's arguments (actual: %d, expected: %d)."
               (length a) (length ns)
eval par fs (Conj s l g) =
  if predicate par s l
    then eval par fs s >>= \r ->
      case r of
        (Nothing, Nothing, log) -> return (Nothing, Nothing, ConjStop:log)
        (Nothing, Just a,  log) -> fillHole g a >>= \s -> return (Just s, Nothing, ConjStopAns:log)
        (Just s', Nothing, log) -> return (Just $ Conj s' (update par s s' log l) g, Nothing, ConjStep:log)
        (Just s', Just a,  log) -> fillHole g a >>= \s'' ->
                              return (Just $ Disj s'' $ Conj s' (update par s s' log l) g, Nothing, ConjStepAns:log)
    else swapConjs par s g >>= \s -> return (Just s, Nothing, [ConjSwap])
eval par fs (Disj p q) = eval par fs p >>= \r ->
  case r of
    (Nothing, Nothing, l) -> return (Just q, Nothing, DisjStop:l)
    (Nothing, Just a,  l) -> return (Just q, Just a, DisjStopAns:l)
    (Just s,  Nothing, l) -> return (Just $ Disj q s, Nothing, DisjStep:l)
    (Just s,  Just a,  l) -> return (Just $ Disj q s, Just a, DisjStepAns:l)

---------------------------------------

printInfo :: Labels l p => Int -> Info -> p -> Stream l -> a -> a
printInfo step (i, swaps) p s =
  if i `mod` step /= 0 then id else
    let dInC = disjsInConjs s in
    trace $ printf
      ("step  : %10d\npath  : %10d\nheight: %10d\nsize  : %10d\ndisjs : %10d\n" ++
       "conjs : %10d\nactCnj: %10d\nd in c: %10d\nmaxLs : %10d\nswaps : %10d\n\n")
      i (path s) (height s) (sizeStream s) (disjCount s) (conjCount s)
      (length dInC) (maximum $ (-1):dInC) (maxSizeLabels p s) swaps

def2fun :: Def -> Fun
def2fun (Def n a g) = (n, (a, g))

prepareAnswer :: Subst -> [X] -> [(X, Ts)]
prepareAnswer s x = map (\(x,i) -> (x, substInT s $ V i)) $ zip x [0..]

data Answers a b = Nil b
                 | a ::: Answers a b

type Info = (Int, Int)

info0 = (0, 0)

type RunAnswers = Answers ([(X, Ts)], Info) (Maybe String, Info)

instance (Show a, Show b) => Show (Answers a b) where
  show a = printf "answers {\n%s" $ show' a where
    show' :: (Show a, Show b) => Answers a b -> String
    show' (Nil b)    = printf "\nmessage: %s\n}\n" $ show b
    show' (x ::: xs) = printf "\n%s\n%s" (show x) $ show' xs

takeAns :: b -> Int -> Answers (a, b) (Maybe String, b) -> Answers (a, b) (Maybe String, b)
takeAns i n _  | n <= 0      = Nil (Just "Zero answers", i)
takeAns _ _ a@(Nil _)        = a
takeAns _ 1 (a@(_, i) ::: _) = a ::: Nil (Just "Enough answers.", i)
takeAns i n (a ::: b)        = a ::: takeAns i (n-1) b

takeAnswers :: Int -> RunAnswers -> RunAnswers
takeAnswers = takeAns info0

initStream :: RunGoal X l -> [X] -> Er (Stream l)
initStream g x = toSemG' (zip x $ map V [0..]) (length x) g >>= \(g, i) -> return $ goal g (i, Eval.sEmpty) where
  toSemG' :: [(X, Ts)] -> Int -> RunGoal X l -> Er (RunGoal S l, Int)
  toSemG' e i (RG g) = toSemG e i g >>= \(g,i) -> return (RG g, i)
  goal :: RunGoal S l -> Subst -> Stream l
  goal (RG g) = Goal g

run :: (Labels l p, Show l) => [X] -> [Def] -> p -> RunGoal X l -> RunAnswers
run vars defs p g =
  case initStream g vars of
    Left msg     -> Nil (Just $ "Error in initial goal. " ++ msg, info0)
    Right s -> run' p info0 (map def2fun defs) s
  where
  run' :: (Labels l p, Show l) => p -> Info -> [Fun] -> Stream l -> RunAnswers
  run' p info@(i, swaps) fs s =
    case eval p fs s of
      Left msg                    -> Nil (Just msg, info)
      Right (Nothing, Nothing, l) -> Nil (Nothing, info)
      Right (Nothing, Just a,  l) -> (prepareAnswer a vars, info) ::: Nil (Nothing, info)
      Right (Just s , Nothing, l) ->
        let swaps' = if last l == ConjSwap then swaps + 1 else swaps in
        printInfo 10000 (i, swaps') p s $
        run' p (i + 1, swaps') fs s
      Right (Just s , Just a, l) ->
        let swaps' = if last l == ConjSwap then swaps + 1 else swaps in
        printInfo 10000 (i, swaps') p s $
        (prepareAnswer a vars, info) ::: run' p (i+1, swaps') fs s
