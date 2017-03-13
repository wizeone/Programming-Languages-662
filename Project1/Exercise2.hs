-- Dominic Pitts
-- Project 2
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
typeOf (Divide t1 t2)   = let t1' = (typeOf t1)
                              t2' = (typeOf t2)
                          in if t1' == (Right Tnum) && t2' == (Right Tnum)
                            then (Right Tnum)
                            else (Left "Type error in /")


optimize :: ABE -> ABE
optimize (Num x)                  = (Num x)
optimize (Boolean b)              = (Boolean b)
optimize (Multiply (Num 0) y)     = (Num 0)
optimize (Multiply x (Num 0))     = (Num 0)
optimize (Multiply (Num 1) y)     = (optimize y)
optimize (Multiply x (Num 1))     = (optimize x)
optimize (Multiply x y)           = let l = (optimize x)
                                        r = (optimize y)
                                    in (Plus l r)
optimize (Divide x (Num 1))       = (optimize x)
optimize (Divide x y)             = let l = (optimize x)
                                        r = (optimize y)
                                    in (Divide l r)
optimize (Plus x (Num 0))         = (optimize x)
optimize (Plus (Num 0) y)         = (optimize y)
optimize (Plus x y)               = let l = (optimize x)
                                        r = (optimize y)
                                    in (Plus l r)
optimize (Minus x (Num 0))        = (optimize x)
optimize (Minus x y)              = let l = (optimize x)
                                        r = (optimize y)
                                    in (Minus l r)
optimize (And (Boolean True) y)   = (optimize y)
optimize (And (Boolean False) _)  = (Boolean False)
optimize (And x y)                = let l = (optimize x)
                                        r = (optimize y)
                                    in (And l r)
optimize (Or  (Boolean True) y)   = (optimize y)
optimize (Or  (Boolean False) y)  = (optimize y)
optimize (Or x y)                 = let l = (optimize x)
                                        r = (optimize y)
                                    in (Or l r)
optimize (Geq x y)                = (Geq (optimize x) (optimize y))
optimize (Leq x y)                = (Leq (optimize x) (optimize y))
optimize (IsZero (Num 0))         = (Boolean True)
optimize (IsZero _)               = (Boolean False)
optimize (If (Boolean True) x _)  = (optimize x)
optimize (If (Boolean False) _ y) = (optimize y)
optimize (If x y z)               = let a = (optimize x)
                                        b = (optimize y)
                                        c = (optimize z)
                                    in (If a b c)



interp :: String -> Either String ABE
interp e = let p = (parseABE e)
           in case (typeOf p) of
             (Right _) -> (eval (optimize p))
             (Left m)  -> (Left m)
