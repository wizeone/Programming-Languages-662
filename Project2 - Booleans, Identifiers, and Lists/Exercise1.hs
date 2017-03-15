-- Dominic Pitts
-- Project 2
-- EECS 662 Programming Languages
{-# LANGUAGE GADTs #-}

import Proj2Utils
import System.IO.Unsafe

evals :: BBAE -> Either String BBAE
evals (Num x) = (Right (Num x))
evals (Plus x y)     = let v1 = (evals x)
                           v2 = (evals y)
                       in case v1 of
                         (Left m) -> v1
                         (Right (Num t1)) -> case v2 of
                                               (Left m) -> v2
                                               (Right (Num t2)) -> (Right (Num (t1 + t2)))
                                               (Right _) -> (Left "Type error in Plus")
evals (Minus x y)    = let v1 = (evals x)
                           v2 = (evals y)
                       in case v1 of
                         (Left m) -> v1
                         (Right (Num t1)) -> case v2 of
                                               (Left m) -> v2
                                               (Right (Num t2)) -> (Right (Num (t1 - t2)))
                                               (Right _) -> (Left "Type error in Minus")
evals (Boolean b)    = (Right (Boolean b))
evals (And x y)      = let v1 = (evals x)
                           v2 = (evals y)
                       in case v1 of
                         (Left m) -> v1
                         (Right (Boolean t1)) -> case v2 of
                                               (Left m) -> v2
                                               (Right (Boolean t2)) -> (Right (Boolean (t1 && t2)))
                                               (Right _) -> (Left "Type error in &&")
evals (Leq x y)      = let v1 = (evals x)
                           v2 = (evals y)
                       in case v1 of
                         (Left m) -> v1
                         (Right (Num t1)) -> case v2 of
                                               (Left m) -> v2
                                               (Right (Num t2)) -> (Right (Boolean (t1 <= t2)))
                                               (Right _) -> (Left "Type error in <=")
evals (IsZero x)     = let r = (evals x)
                       in case r of
                         (Left m) -> r
                         (Right (Num v)) -> (Right (Boolean (v == 0)))
                         (Right _) -> (Left "Type error for isZero")
evals (If x y z)     = let r = (evals x)
                       in case r of
                         (Left _) -> r
                         (Right (Boolean s)) -> if s then (evals y) else (evals z)
                         (Right _) -> (Left "Type error in if")
evals (Bind i v b)   = do {
                            v' <- evals v;
                            b' <- (evals (subst i v' b));
                            return b'
                          }

  -- let v' = evals v
  --                      in (evals (subst i v' b))
evals (Seq l r)      = let l' = (evals l)
                       in case l' of
                         (Left m) -> l'
                         (Right _) -> let r' = (evals r)
                                      in case r' of
                                        (Left m') -> (Left "Error in Seq")
                                        (Right _) -> r'
evals (Print x)      = (seq (unsafePerformIO (print (evals x))) (Right (Num 0)))

eval :: BBAE -> Either String BBAE
eval (Num x) = (Right (Num x))
eval (Plus x y)     = let v1 = (eval x)
                          v2 = (eval y)
                      in case v1 of
                        (Left m) -> v1
                        (Right (Num t1)) -> case v2 of
                                              (Left m) -> v2
                                              (Right (Num t2)) -> (Right (Num (t1 + t2)))
                                              (Right _) -> (Left "Type error in Plus")
eval (Minus x y)    = let v1 = (eval x)
                          v2 = (eval y)
                      in case v1 of
                        (Left m) -> v1
                        (Right (Num t1)) -> case v2 of
                                              (Left m) -> v2
                                              (Right (Num t2)) -> (Right (Num (t1 - t2)))
                                              (Right _) -> (Left "Type error in Minus")
eval (Boolean b)    = (Right (Boolean b))
eval (And x y)      = let v1 = (eval x)
                          v2 = (eval y)
                      in case v1 of
                        (Left m) -> v1
                        (Right (Boolean t1)) -> case v2 of
                                              (Left m) -> v2
                                              (Right (Boolean t2)) -> (Right (Boolean (t1 && t2)))
                                              (Right _) -> (Left "Type error in &&")
eval (Leq x y)      = let v1 = (eval x)
                          v2 = (eval y)
                      in case v1 of
                        (Left m) -> v1
                        (Right (Num t1)) -> case v2 of
                                              (Left m) -> v2
                                              (Right (Num t2)) -> (Right (Boolean (t1 <= t2)))
                                              (Right _) -> (Left "Type error in <=")
eval (IsZero x)     = let r = (eval x)
                      in case r of
                        (Left m) -> r
                        (Right (Num v)) -> (Right (Boolean (v == 0)))
                        (Right _) -> (Left "Type error for isZero")
eval (If x y z)     = let r = (eval x)
                      in case r of
                        (Left _) -> r
                        (Right (Boolean s)) -> if s then (eval y) else (eval z)
                        (Right _) -> (Left "Type error in if")
eval (Seq l r)      = let l' = (eval l)
                      in case l' of
                       (Left m) -> l'
                       (Right _) -> let r' = (evals r)
                                    in case r' of
                                      (Left m') -> (Left "Error in Seq")
                                      (Right _) -> r'
eval (Print x)      = (seq (unsafePerformIO (print (evals x))) (Right (Num 0)))

-- Pulled from text book
subst :: String -> BBAE -> BBAE -> BBAE
subst _ _ (Num x) = (Num x)
subst i v (Plus l r) = (Plus (subst i v l) (subst i v r))
subst i v (Minus l r) = (Minus (subst i v l) (subst i v r))
subst i v (Bind i' v' b') = if i == i'
	                          then (Bind i' (subst i v v') b')
	                          else (Bind i' (subst i v v') (subst i v b'))
subst i v (Id i') = if i==i'
  	                then v
  	                else (Id i')


interpEvals :: String -> Either String BBAE
interpEvals = evals . parseBBAE

interpEval :: String -> Either String BBAE
interpEval = eval . parseBBAE
