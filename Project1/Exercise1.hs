-- Dominic Pitts
-- Project 1
-- EECS 662 Programming Languages
{-# LANGUAGE GADTs #-}

import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token
import ParserUtils

data ABE where
  Num      :: Int -> ABE
  Plus     :: ABE -> ABE -> ABE
  Minus    :: ABE -> ABE -> ABE
  Divide   :: ABE -> ABE -> ABE
  Multiply :: ABE -> ABE -> ABE
  Boolean  :: Bool -> ABE
  And      :: ABE -> ABE -> ABE
  Or       :: ABE -> ABE -> ABE
  Leq      :: ABE -> ABE -> ABE
  Geq      :: ABE -> ABE -> ABE
  IsZero   :: ABE -> ABE
  If       :: ABE -> ABE -> ABE -> ABE
  deriving (Show,Eq)

data TABE where
  Tnum   :: TABE
  Tbool  :: TABE
  deriving (Show,Eq)

parseABE :: String -> ABE
parseABE = parseString expr

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

ifExpr :: Parser ABE
ifExpr = do reserved lexer "if"
            c <- expr
            reserved lexer "then"
            t <- expr
            reserved lexer "else"
            e <- expr
            return (If c t e)

numExpr :: Parser ABE
numExpr = do i <- integer lexer
             return (Num (fromInteger i))

tExpr :: Parser ABE
tExpr = do i <- reserved lexer "true"
           return (Boolean True)

fExpr :: Parser ABE
fExpr = do i <- reserved lexer "false"
           return (Boolean False)

term = parens lexer expr
        <|> numExpr
        <|> tExpr
        <|> fExpr
        <|> ifExpr

expr :: Parser ABE
expr = buildExpressionParser operators term

eval :: ABE -> Either String ABE
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

typeOf :: ABE -> Either String TABE
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


interp :: String -> Either String ABE
interp e = let p = (parseABE e)
           in case (typeOf p) of
             (Right _) -> (eval p)
             (Left m)  -> (Left m)
