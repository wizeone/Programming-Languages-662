-- Dominic Pitts
-- Project 2
-- EECS 662 Programming Languages
{-# LANGUAGE GADTs #-}

import Proj2Utils
import System.IO.Unsafe

type Env = [(String,BBAE)]

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
evals (Seq l r)      = let l' = (evals l)
                       in case l' of
                         (Left m) -> l'
                         (Right _) -> let r' = (evals r)
                                      in case r' of
                                        (Left m') -> (Left "Error in Seq")
                                        (Right _) -> r'
evals (Print x)      = (seq (unsafePerformIO (print (evals x))) (Right (Num 0)))
evals (Id id)        = error "Undeclared Variable"
evals (First (Cons x y))   = (Right x)
evals (Rest  (Cons x y))   = (Right y)
evals (Cons x y)           = let (Right x') = (evals x)
                                 (Right y') = (evals y)
                             in (Right (Cons x' y'))
evals Empty                = (Right Empty)
evals (IsEmpty Empty)      = (Right (Boolean True))
evals (IsEmpty (Cons _ _)) = (Right (Boolean False))

eval :: Env -> BBAE -> Either String BBAE
eval env (Num x) = (Right (Num x))
eval env (Plus x y)     = let v1 = (eval env x)
                              v2 = (eval env y)
                          in case v1 of
                            (Left m) -> v1
                            (Right (Num t1)) -> case v2 of
                                                  (Left m) -> v2
                                                  (Right (Num t2)) -> (Right (Num (t1 + t2)))
                                                  (Right _) -> (Left "Type error in Plus")
eval env (Minus x y)    = let v1 = (eval env x)
                              v2 = (eval env y)
                          in case v1 of
                            (Left m) -> v1
                            (Right (Num t1)) -> case v2 of
                                                  (Left m) -> v2
                                                  (Right (Num t2)) -> (Right (Num (t1 - t2)))
                                                  (Right _) -> (Left "Type error in Minus")
eval env (Boolean b)    = (Right (Boolean b))
eval env (And x y)      = let v1 = (eval env x)
                              v2 = (eval env y)
                          in case v1 of
                            (Left m) -> v1
                            (Right (Boolean t1)) -> case v2 of
                                                  (Left m) -> v2
                                                  (Right (Boolean t2)) -> (Right (Boolean (t1 && t2)))
                                                  (Right _) -> (Left "Type error in &&")
eval env (Leq x y)      = let v1 = (eval env x)
                              v2 = (eval env y)
                          in case v1 of
                            (Left m) -> v1
                            (Right (Num t1)) -> case v2 of
                                                  (Left m) -> v2
                                                  (Right (Num t2)) -> (Right (Boolean (t1 <= t2)))
                                                  (Right _) -> (Left "Type error in <=")
eval env (IsZero x)     = let r = (eval env x)
                          in case r of
                            (Left m) -> r
                            (Right (Num v)) -> (Right (Boolean (v == 0)))
                            (Right _) -> (Left "Type error for isZero")
eval env (If x y z)     = let r = (eval env x)
                          in case r of
                            (Left _) -> r
                            (Right (Boolean s)) -> if s
                                                   then (eval env y)
                                                   else (eval env z)
                            (Right _) -> (Left "Type error in if")
eval env (Bind i v b)   = let (Right v') = eval env v
                          in (eval ((i,v'):env) b)
eval env (Seq l r)      = let l' = (eval env l)
                          in case l' of
                           (Left m) -> l'
                           (Right _) -> let r' = (eval env r)
                                        in case r' of
                                          (Left m') -> (Left "Error in Seq")
                                          (Right _) -> r'
eval env (Print x)      = (seq (unsafePerformIO (print (eval env x))) (Right (Num 0)))
eval env (Id i)         = case (lookup i env) of
                            Just x -> (Right x)
                            Nothing -> error "Variable not found"
eval env (First (Cons x y))   = (Right x)
eval env (Rest  (Cons x y))   = (Right y)
eval env (Cons x y)           = let (Right x') = (eval env x)
                                    (Right y') = (eval env y)
                                in (Right (Cons x' y'))
eval env Empty                = (Right Empty)
eval env (IsEmpty Empty)      = (Right (Boolean True))
eval env (IsEmpty (Cons _ _)) = (Right (Boolean False))

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
interpEval x = eval [] (parseBBAE x)
