-- Dominic Pitts
-- Project 2
-- EECS 662 Programming Languages
{-# LANGUAGE GADTs #-}

import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token
import Proj2Utils

data BBAE where
  Num      :: Int -> BBAE
  Plus     :: BBAE -> BBAE -> BBAE
  Minus    :: BBAE -> BBAE -> BBAE
  Divide   :: BBAE -> BBAE -> BBAE
  Multiply :: BBAE -> BBAE -> BBAE
  Boolean  :: Bool -> BBAE
  And      :: BBAE -> BBAE -> BBAE
  Or       :: BBAE -> BBAE -> BBAE
  Leq      :: BBAE -> BBAE -> BBAE
  Geq      :: BBAE -> BBAE -> BBAE
  IsZero   :: BBAE -> BBAE
  If       :: BBAE -> BBAE -> BBAE -> BBAE
  Seq      :: BBAE -> BBAE -> BBAE
  Print    :: BBAE -> BBAE
  deriving (Show,Eq)

data TBBAE where
  Tnum   :: TBBAE
  Tbool  :: TBBAE
  deriving (Show,Eq)

parseBBAE :: String -> BBAE
parseBBAE = parseString expr

operators = [ [ inFix "*" Multiply AssocLeft
              , inFix "/" Divide AssocLeft
              , inFix "+" Plus AssocLeft
              , inFix "-" Minus AssocLeft
              ],
              [ inFix "<=" Leq AssocLeft
              , inFix ">=" Geq AssocLeft
              , preFix "isZero" IsZero
              ],
              [ inFix "&&" And AssocLeft
              , inFix "||" Or AssocLeft ] ]

ifExpr :: Parser BBAE
ifExpr = do reserved lexer "if"
            c <- expr
            reserved lexer "then"
            t <- expr
            reserved lexer "else"
            e <- expr
            return (If c t e)

numExpr :: Parser BBAE
numExpr = do i <- integer lexer
             return (Num (fromInteger i))

tExpr :: Parser BBAE
tExpr = do i <- reserved lexer "true"
           return (Boolean True)

fExpr :: Parser BBAE
fExpr = do i <- reserved lexer "false"
           return (Boolean False)

term = parens lexer expr
        <|> numExpr
        <|> tExpr
        <|> fExpr
        <|> ifExpr

expr :: Parser BBAE
expr = buildExpressionParser operators term

evals :: BBAE -> Either String BBAE
evals (Num x) = (Right (Num x))
evals (Multiply x y) = let v1 = (evals x)
                          v2 = (evals y)
                      in case v1 of
                        (Left m) -> v1
                        (Right (Num t1)) -> case v2 of
                                              (Left m) -> v2
                                              (Right (Num t2)) -> (Right (Num (t1 * t2)))
                                              (Right _) -> (Left "Type error in *")
evals (Divide x y)   = let v1 = (evals x)
                          v2 = (evals y)
                      in case v1 of
                        (Left m) -> v1
                        (Right (Num t1)) -> case v2 of
                                              (Left m) -> v2
                                              (Right (Num t2)) -> (Right (Num (t1 `div` t2)))
                                              (Right _) -> (Left "Type error in Divide")
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
evals (Or x y)       = let v1 = (evals x)
                          v2 = (evals y)
                      in case v1 of
                        (Left m) -> v1
                        (Right (Boolean t1)) -> case v2 of
                                              (Left m) -> v2
                                              (Right (Boolean t2)) -> (Right (Boolean (t1 || t2)))
                                              (Right _) -> (Left "Type error in ||")
evals (Leq x y)      = let v1 = (evals x)
                          v2 = (evals y)
                      in case v1 of
                        (Left m) -> v1
                        (Right (Num t1)) -> case v2 of
                                              (Left m) -> v2
                                              (Right (Num t2)) -> (Right (Boolean (t1 <= t2)))
                                              (Right _) -> (Left "Type error in <=")
evals (Geq x y)      = let v1 = (evals x)
                          v2 = (evals y)
                      in case v1 of
                        (Left m) -> v1
                        (Right (Num t1)) -> case v2 of
                                              (Left m) -> v2
                                              (Right (Num t2)) -> (Right (Boolean (t1 >= t2)))
                                              (Right _) -> (Left "Type error in >=")
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

eval :: BBAE -> Either String BBAE
eval (Num x) = (Right (Num x))
eval (Multiply x y) = let v1 = (eval x)
                          v2 = (eval y)
                      in case v1 of
                        (Left m) -> v1
                        (Right (Num t1)) -> case v2 of
                                              (Left m) -> v2
                                              (Right (Num t2)) -> (Right (Num (t1 * t2)))
                                              (Right _) -> (Left "Type error in *")
eval (Divide x y)   = let v1 = (eval x)
                          v2 = (eval y)
                      in case v1 of
                        (Left m) -> v1
                        (Right (Num t1)) -> case v2 of
                                              (Left m) -> v2
                                              (Right (Num t2)) -> (Right (Num (t1 `div` t2)))
                                              (Right _) -> (Left "Type error in Divide")
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
eval (Or x y)       = let v1 = (eval x)
                          v2 = (eval y)
                      in case v1 of
                        (Left m) -> v1
                        (Right (Boolean t1)) -> case v2 of
                                              (Left m) -> v2
                                              (Right (Boolean t2)) -> (Right (Boolean (t1 || t2)))
                                              (Right _) -> (Left "Type error in ||")
eval (Leq x y)      = let v1 = (eval x)
                          v2 = (eval y)
                      in case v1 of
                        (Left m) -> v1
                        (Right (Num t1)) -> case v2 of
                                              (Left m) -> v2
                                              (Right (Num t2)) -> (Right (Boolean (t1 <= t2)))
                                              (Right _) -> (Left "Type error in <=")
eval (Geq x y)      = let v1 = (eval x)
                          v2 = (eval y)
                      in case v1 of
                        (Left m) -> v1
                        (Right (Num t1)) -> case v2 of
                                              (Left m) -> v2
                                              (Right (Num t2)) -> (Right (Boolean (t1 >= t2)))
                                              (Right _) -> (Left "Type error in >=")
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

typeOf :: BBAE -> Either String TBBAE
typeOf (Num _)        = (Right Tnum)
typeOf (Plus t1 t2)   = let t1' = (typeOf t1)
                            t2' = (typeOf t2)
                        in if t1' == (Right Tnum) && t2' == (Right Tnum)
                          then (Right Tnum)
                          else (Left "Type error in +")
typeOf (Minus t1 t2)  = let t1' = (typeOf t1)
                            t2' = (typeOf t2)
                        in if t1' == (Right Tnum) && t2' == (Right Tnum)
                          then (Right Tnum)
                          else (Left "Type error in -")
typeOf (Boolean _)    = (Right Tbool)
typeOf (IsZero t)     = if (typeOf t) == (Right Tnum)
                        then (Right Tbool)
                        else Left "Type error in IsZero"
typeOf (And l r)      = if (typeOf l) == (Right Tbool)
                           && (typeOf r) == (Right Tbool)
                        then (Right Tbool)
                        else Left "Type error in &&"
typeOf (Or l r)       = if (typeOf l) == (Right Tbool)
                           || (typeOf r) == (Right Tbool)
                        then (Right Tbool)
                        else Left "Type error in ||"
typeOf (Leq l r)      = if (typeOf l) == (Right Tnum)
                           && (typeOf r) == (Right Tnum)
                        then (Right Tbool)
                        else Left "Type error in <="
typeOf (Geq l r)      = if (typeOf l) == (Right Tnum) && (typeOf r) == (Right Tnum)
                        then (Right Tbool)
                        else Left "Type error in >="
typeOf (If t1 t2 t3)  = if (typeOf t1) == (Right Tbool) && (typeOf t2) == (typeOf t3)
                        then (typeOf t2)
                        else Left "Type error in if"
typeOf (Multiply t1 t2) = let t1' = (typeOf t1)
                              t2' = (typeOf t2)
                          in if t1' == (Right Tnum) && t2' == (Right Tnum)
                            then (Right Tnum)
                            else (Left "Type error in *")
typeOf (Divide t1 t2) = let t1' = (typeOf t1)
                            t2' = (typeOf t2)
                        in if t1' == (Right Tnum) && t2' == (Right Tnum)
                          then (Right Tnum)
                          else (Left "Type error in /")

-- Pulled from text book
subst :: String -> BBAE -> BBAE -> BBAE
subst _ _ (Num x) = (Num x)
subst i v (Plus l r) = (Plus (subst i v l) (subst i v r))
subst i v (Minus l r) = (Minus (subst i v l) (subst i v r))
subst i v (Bind i' v' b') = if i==i'
	                          then (Bind i' (subst i v v') b')
	                          else (Bind i' (subst i v v') (subst i v b'))
subst i v (Id i') = if i==i'
  	                then v
  	                else (Id i')


interp :: String -> Either String BBAE
interp e = let p = (parseBBAE e)
           in case (typeOf p) of
             (Right _) -> (evals p)
             (Left m)  -> (Left m)
