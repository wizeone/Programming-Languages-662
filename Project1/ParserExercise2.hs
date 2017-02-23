-- Dominic Pitts
{-# LANGUAGE GADTs #-}

import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token
import ParserUtils

data AEE where
  Num      :: Int -> AEE
  Plus     :: AEE -> AEE -> AEE
  Minus    :: AEE -> AEE -> AEE
  Divide   :: AEE -> AEE -> AEE
  Multiply :: AEE -> AEE -> AEE
  Boolean  :: Bool -> AEE
  And      :: AEE -> AEE -> AEE
  Or       :: AEE -> AEE -> AEE
  Leq      :: AEE -> AEE -> AEE
  Geq      :: AEE -> AEE -> AEE
  IsZero   :: AEE -> AEE
  If       :: AEE -> AEE -> AEE -> AEE
  deriving (Show,Eq)

expr :: Parser AEE
expr = buildExpressionParser operators term

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

ifExpr :: Parser AEE
ifExpr = do reserved lexer "if"
            c <- expr
            reserved lexer "then"
            t <- expr
            reserved lexer "else"
            e <- expr
            return (If c t e)

numExpr :: Parser AEE
numExpr = do i <- integer lexer
             return (Num (fromInteger i))

tExpr :: Parser AEE
tExpr = do i <- reserved lexer "true"
           return (Boolean True)

fExpr :: Parser AEE
fExpr = do i <- reserved lexer "false"
           return (Boolean False)

term = parens lexer expr
        <|> numExpr
        <|> tExpr
        <|> fExpr
        <|> ifExpr

parseAEE = parseString expr

pareseAEEFile = parseFile expr

eval :: AEE -> AEE
eval (Num x) = (Num x)
eval (Multiply x y) = let (Num v1) = (eval x)
                          (Num v2) = (eval y)
                      in (Num (v1 * v2))
eval (Divide x y)   = let (Num v1) = (eval x)
                          (Num v2) = (eval y)
                      in (Num (v1 `div` v2))
eval (Plus x y)     = let (Num v1) = (eval x)
                          (Num v2) = (eval y)
                      in (Num (v1 + v2))
eval (Minus x y)    = let (Num v1) = (eval x)
                          (Num v2) = (eval y)
                      in (Num (v1 - v2))
eval (Boolean b)    = (Boolean b)
eval (And x y)      = let (Boolean v1) = (eval x)
                          (Boolean v2) = (eval y)
                      in (Boolean (v1 && v2))
eval (Or x y)       = let (Boolean v1) = (eval x)
                          (Boolean v2) = (eval y)
                      in (Boolean (v1 || v2))
eval (Leq x y)      = let (Num v1) = (eval x)
                          (Num v2) = (eval y)
                      in (Boolean (v1 <= v2))
eval (Geq x y)      = let (Num v1) = (eval x)
                          (Num v2) = (eval y)
                      in (Boolean (v1 >= v2))
eval (IsZero x)     = let (Num v) = (eval x)
                      in (Boolean (v == 0))
eval (If x y z)     = let (Boolean v) = (eval x)
                      in if v then (eval y) else (eval z)

interp :: String -> AEE
interp = eval . parseAEE
