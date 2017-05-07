-- Dominic Pitts
-- Project 3
-- EECS 662 Programming Languages
{-# LANGUAGE GADTs #-}

import Proj4Utils
import System.IO.Unsafe

type Env = [(String,FBAE)]
type EnvS = [(String,CFBAValue)]
type Cont = [(String,TFBAE)]

data CFBAValue where
  NumV :: Int -> CFBAValue
  BooleanV :: Bool -> CFBAValue
  ClosureV :: String -> FBAE -> EnvS -> CFBAValue
  deriving (Show,Eq)

subst :: String -> FBAE -> FBAE -> FBAE
subst _ _ (Num x) = (Num x)
subst i v (Plus l r) = (Plus (subst i v l) (subst i v r))
subst i v (Minus l r) = (Minus (subst i v l) (subst i v r))
subst i v (Mult l r) = (Mult (subst i v l) (subst i v r))
subst i v (Div l r) = (Div (subst i v l) (subst i v r))
subst i v (Bind i' v' b') = if i == i'
                            then (Bind i' (subst i v v') b')
                            else (Bind i' (subst i v v') (subst i v b'))
subst i v (Lambda i' d b) = (Lambda i' d (subst i v b))
subst i v (App f a) = (App (subst i v f) (subst i v a))
subst i v (Id i') = if i==i'
                    then v
                    else (Id i')
subst i v (Boolean x) = (Boolean x)
subst i v (And l r) = (And (subst i v l) (subst i v r))
subst i v (Or l r) = (Or (subst i v l) (subst i v r))
subst i v (Leq l r) = (Leq (subst i v l) (subst i v r))
subst i v (IsZero x) = (IsZero (subst i v x))
subst i v (If x y z) = (If (subst i v x) (subst i v y) (subst i v z))
subst i v (Fix f) = (Fix (subst i v f))

eval :: EnvS -> FBAE -> CFBAValue
eval env (Num x) = (NumV x)
eval env (Plus l r) = let (NumV l') = eval env l
                          (NumV r') = eval env r
                      in (NumV (l' + r'))
eval env (Minus l r) = let (NumV l') = eval env l
                           (NumV r') = eval env r
                       in (NumV (l' - r'))
eval env (Mult l r) = let (NumV l') = eval env l
                          (NumV r') = eval env r
                      in (NumV (l' * r'))
eval env (Div l r) = let (NumV l') = eval env l
                         (NumV r') = eval env r
                     in if (r' == 0) then error "Dividing by zero."
                                     else (NumV (div l' r'))
eval env (Bind i v b)   = eval env (App (Lambda i (TNum) b) v)
eval env (Lambda i d b) = (ClosureV i b env)
eval env (App f a) = let (ClosureV i b e) = (eval env f)
                         a' = (eval env a)
                     in eval ((i,a'):e) b
eval env (Id i) = case (lookup i env) of
                            Just x -> x
                            Nothing -> error "Id not found."
eval env (Boolean x) = (BooleanV x)
eval env (And l r) = let (BooleanV l') = eval env l
                         (BooleanV r') = eval env r
                     in (BooleanV (l' && r'))
eval env (Or l r) = let (BooleanV l') = eval env l
                        (BooleanV r') = eval env r
                    in (BooleanV (l' || r'))
eval env (Leq l r) = let (NumV l') = eval env l
                         (NumV r') = eval env r
                     in (BooleanV (l' <= r'))
eval env (IsZero x) = let (NumV x') = eval env x
                      in (BooleanV (x' == 0))
eval env (If x y z) = let (BooleanV x') = eval env x
                      in if x' then (eval env y) else (eval env z)
eval env (Fix f) = let (ClosureV i b e) = eval env f
                   in eval e (subst i (Fix (Lambda i (TNum) b)) b)

typeof :: Cont -> FBAE -> Either String TFBAE
typeof cont (Num _) = (Right TNum)
typeof cont (Plus l r) = let t1 = (typeof cont l)
                             t2 = (typeof cont r)
                         in if t1 == (Right TNum) && t2 == (Right TNum)
                            then (Right TNum)
                            else (Left "Type mismatch in Plus.")
typeof cont (Minus l r) = let t1 = (typeof cont l)
                              t2 = (typeof cont r)
                          in if t1 == (Right TNum) && t2 == (Right TNum)
                             then (Right TNum)
                             else (Left "Type mismatch in Minus.")
typeof cont (Mult l r) = let t1 = (typeof cont l)
                             t2 = (typeof cont r)
                         in if t1 == (Right TNum) && t2 == (Right TNum)
                            then (Right TNum)
                            else (Left "Type mismatch in Multiply.")
typeof cont (Div l r) = let t1 = (typeof cont l)
                            t2 = (typeof cont r)
                        in if t1 == (Right TNum) && t2 == (Right TNum)
                           then (Right TNum)
                           else (Left "Type mismatch in Divide.")
typeof cont (Bind i v b) = let (Right v') = (typeof cont v)
                           in (typeof ((i,v'):cont) b)
typeof cont (Lambda i d b) = let (Right r) = typeof ((i,d):cont) b
                             in (Right (d:->:r))
typeof cont (App f a) = let a' = typeof cont a
                        in case typeof cont f of
                            (Right (tyXd:->:tyXr)) ->
                              if (Right tyXd) == a'
                              then (Right tyXr)
                              else (Left "Type mismatch in App.")
                            (Left _) -> (Left "First argument not lambda in app.")
typeof cont (Id i) = case (lookup i cont) of
                      Just x -> (Right x)
                      Nothing -> (Left "Variable not found.")
typeof cont (Boolean _) = (Right TBool)
typeof cont (And l r) = let l' = typeof cont l
                            r' = typeof cont r
                        in if l' == (Right TBool) && r' == (Right TBool)
                           then (Right TBool)
                           else (Left "Type mismatch in And.")
typeof cont (Or l r) = let l' = typeof cont l
                           r' = typeof cont r
                       in if l' == (Right TBool) && r' == (Right TBool)
                          then (Right TBool)
                          else (Left "Type mismatch in Or.")
typeof cont (Leq l r) = let l' = typeof cont l
                            r' = typeof cont r
                        in if l' == (Right TNum) && r' == (Right TNum)
                           then (Right TBool)
                           else (Left "Type mismatch in Leq.")
typeof cont (IsZero x) = if (typeof cont x) == (Right TNum)
                         then (Right TBool)
                         else (Left "Type mismatch in IsZero.")
typeof cont (If x y z) = let x' = typeof cont x
                         in if x' == (Right TBool)
                            then let y' = typeof cont y
                                     z' = typeof cont z
                                 in if y' == z'
                                    then y'
                                    else (Left "Type mismatch in If.")
                            else (Left "If check isn't a boolean.")
typeof cont (Fix f) = let (Right (r:->:d)) = typeof cont f
                      in (Right d)

interp :: String -> CFBAValue
interp e = let p = (parseFBAE e)
           in case (typeof [] p) of
             (Right _) -> (eval [] p)
             (Left m)  -> error m
