-- Dominic Pitts
-- Project 3
-- EECS 662 Programming Languages
{-# LANGUAGE GADTs #-}

import Proj4Utils
import System.IO.Unsafe

type Env = [(String,FBAE)]
type EnvS = [(String,CFBAValue)]

-----------------------------------
--      Exercise 1
-----------------------------------

data CFBAValue where
  NumV :: Int -> CFBAValue
  BooleanV :: Bool -> FBAEVal
  ClosureV :: String -> FBAE -> EnvS -> CFBAValue
  deriving (Show,Eq)

subst :: String -> BBAE -> BBAE -> BBAE
subst _ _ (Num x) = (Num x)
subst i v (Plus l r) = (Plus (subst i v l) (subst i v r))
subst i v (Minus l r) = (Minus (subst i v l) (subst i v r))
subst i v (Mult l r) = (Mult (subst i v l) (subst i v r))
subst i v (Div l r) = (Div (subst i v l) (subst i v r))
subst i v (Bind i' v' b') = if i == i'
                            then (Bind i' (subst i v v') b')
                            else (Bind i' (subst i v v') (subst i v b'))
subst i v (Lambda i b) =
subst i v (Id i') = if i==i'
                    then v
                    else (Id i')

eval :: EnvS -> FBAE -> CFBAValue
eval env (Num x) = (NumV x)
eval env (Plus l r) = let (Num l') = eval env l
                          (Num r') = eval env r
                      in (NumV (l' + r'))
eval env (Minus l r) = let (Num l') = eval env l
                           (Num r') = eval env r
                       in (NumV (l' - r'))
eval env (Mult l r) = let (Num l') = eval env l
                          (Num r') = eval env r
                      in (NumV (l' * r'))
eval env (Div l r) = let (Num l') = eval env l
                         (Num r') = eval env r
                     in if (r' == 0) then error "Dividing by zero."
                                     else (NumV (div l' r'))
eval env (Bind i v b)   = do {
                               v' <- eval env v;
                               b' <- (eval env (subst i v' b));
                               return b'
                             }
eval env (Lambda i d b) = (ClosureV i b env)
eval env (App f a) = let (ClosureV i b e) = (eval env f)
                         a' = (eval env a)
                     in eval ((i,a'):e) b
eval env (Id x) = case (lookup i env) of
                            Just x -> x
                            Nothing -> error "Id not found."
eval env (Bool x) = (BooleanV x)
eval env (And l r) = let (Boolean l') = eval env l
                         (Boolean r') = eval env r
                     in (BooleanV (l' && r'))
eval env (Or l r) = let (Boolean l') = eval env l
                        (Boolean r') = eval env r
                    in (BooleanV (l' || r'))
eval env (Leq l r) = let (Boolean l') = eval env l
                         (Boolean r') = eval env r
                     in (BooleanV (l' <= r'))
eval env (IsZero x) = let (Num x') = eval env x
                      in (BooleanV (x' == 0))
eval env (If x y z) = let (Boolean x') = eval env x
                      in if x' then (eval env y) else (eval env z)
eval env (Fix f) = let (ClosureV i b e) = eval env f
                   in eval e (subst i (Fix (lambda i b)) b)
