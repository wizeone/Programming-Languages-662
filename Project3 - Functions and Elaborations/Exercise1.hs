-- Dominic Pitts
-- Project 3
-- EECS 662 Programming Languages
{-# LANGUAGE GADTs #-}

import Proj3Utils
import System.IO.Unsafe

type Env = [(String,CFAE)]

---------------------------
--      EXERCISE 1
---------------------------

evalDynCFAE :: Env -> CFAE -> CFAE
evalDynCFAE env (Num x) = (Num x)
evalDynCFAE env (Plus l r) = let (Num l') = (evalDynCFAE env l)
                                 (Num r') = (evalDynCFAE env r)
                             in (Num (l' + r'))
evalDynCFAE env (Minus l r) = let (Num l') = (evalDynCFAE env l)
                                  (Num r') = (evalDynCFAE env r)
                              in (Num (l' - r'))
evalDynCFAE env (Mult l r) = let (Num l') = (evalDynCFAE env l)
                                 (Num r') = (evalDynCFAE env r)
                             in (Num (l' * r'))
evalDynCFAE env (Div l r) = let (Num l') = (evalDynCFAE env l)
                                (Num r') = (evalDynCFAE env r)
                            in (Num (div l' r'))
evalDynCFAE env (Lambda i b) = (Lambda i b)
evalDynCFAE env (App f a) = let (Lambda i b) = (evalDynCFAE env f)
                                a' = (evalDynCFAE env a)
                            in (evalDynCFAE ((i,a'):env) b)
evalDynCFAE env (Id i) = case (lookup i env) of
                          Just x -> x
                          Nothing -> error "Id not found."
evalDynCFAE env (If x y z) = let (Num x') = (evalDynCFAE env x)
                             in if x' == 0 then (evalDynCFAE env y)
                                           else (evalDynCFAE env z)

interpDynCFAE :: String -> CFAE
interpDynCFAE s = evalDynCFAE [] (parseCFAE s)

---------------------------
--      EXERCISE 2
---------------------------

type EnvS = [(String,CFAEValue)]

data CFAEValue where
  NumV :: Int -> CFAEValue
  ClosureV :: String -> CFAE -> EnvS -> CFAEValue
  deriving (Show,Eq)

evalStatCFAE :: EnvS -> CFAE -> CFAEValue
evalStatCFAE env (Num x) = (NumV x)
evalStatCFAE env (Plus l r) = let (NumV l') = (evalStatCFAE env l)
                                  (NumV r') = (evalStatCFAE env r)
                              in (NumV (l' + r'))
evalStatCFAE env (Minus l r) = let (NumV l') = (evalStatCFAE env l)
                                   (NumV r') = (evalStatCFAE env r)
                               in (NumV (l' - r'))
evalStatCFAE env (Mult l r) = let (NumV l') = (evalStatCFAE env l)
                                  (NumV r') = (evalStatCFAE env r)
                              in (NumV (l' * r'))
evalStatCFAE env (Div l r) = let (NumV l') = (evalStatCFAE env l)
                                 (NumV r') = (evalStatCFAE env r)
                             in (NumV (div l' r'))
evalStatCFAE env (Lambda i b) = (ClosureV i b env)
evalStatCFAE env (App f a) = let (ClosureV i b e) = (evalStatCFAE env f)
                                 a' = (evalStatCFAE env a)
                             in evalStatCFAE ((i,a'):e) b
evalStatCFAE env (Id i) = case (lookup i env) of
                            Just x -> x
                            Nothing -> error "Id not found."
evalStatCFAE env (If x y z) = let (NumV x') = (evalStatCFAE env x)
                              in if x' == 0 then (evalStatCFAE env y)
                                            else (evalStatCFAE env z)

interpStatCFAE :: String -> CFAEValue
interpStatCFAE s = evalStatCFAE [] (parseCFAE s)


---------------------------
--      EXERCISE 3
---------------------------

elabCFBAE :: CFBAE -> CFAE
elabCFBAE (NumX x) = (Num x)
elabCFBAE (PlusX l r) = (Plus (elabCFBAE l) (elabCFBAE r))
elabCFBAE (MinusX l r) = (Minus (elabCFBAE l) (elabCFBAE r))
elabCFBAE (MultX l r) = (Mult (elabCFBAE l) (elabCFBAE r))
elabCFBAE (DivX l r) = (Div (elabCFBAE l) (elabCFBAE r))
elabCFBAE (BindX i v b) = let v' = (elabCFBAE v)
                              b' = (elabCFBAE b)
                          in (App (Lambda i b') v')
elabCFBAE (LambdaX i b) = let b' = (elabCFBAE b)
                          in (Lambda i b')
elabCFBAE (AppX f a) = let f' = (elabCFBAE f)
                           a' = (elabCFBAE a)
                       in (App f' a')
elabCFBAE (IdX i) = (Id i)
elabCFBAE (IfX x y z) = let x' = (elabCFBAE x)
                            y' = (elabCFBAE y)
                            z' = (elabCFBAE z)
                        in (If x' y' z')

evalCFBAE :: EnvS -> CFBAE -> CFAEValue
evalCFBAE env x = (evalStatCFAE env (elabCFBAE x))

-- interpCFBAE :: String -> CFAEValue
-- interpCFBAE x = (evalCFBAE [("inc",(App (ClosureV (Id n) (Plus (Id n) (Num 1))))),
--                             ("dec",(App (ClosureV (Id n) (Plus (Id n) (Num 1)))))]
--                            (elabCFBAE x))

interpCFBAE :: String -> CFAEValue
interpCFBAE x = (evalCFBAE []
                           (parseCFBAE x))
