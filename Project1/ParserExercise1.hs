-- Dominic Pitts

{-# LANGUAGE GADTs #-}

import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token
import ParserUtils

data AE where
  Num      :: Int -> AE
  Plus     :: AE -> AE -> AE
  Minus    :: AE -> AE -> AE
  Divide   :: AE -> AE -> AE
  Multiply :: AE -> AE -> AE
  deriving (Show,Eq)

expr :: Parser AE
expr = buildExpressionParser operators term

operators = [ [ inFix "*" Multiply AssocLeft
              , inFix "/" Divide AssocLeft
              , inFix "+" Plus AssocLeft
              , inFix "-" Minus AssocLeft
              ] ]

numExpr :: Parser AE
numExpr = do i <- integer lexer
             return (Num (fromInteger i))

term = parens lexer expr <|> numExpr

parseAE = parseString expr

pareseAEFile = parseFile expr

eval :: AE -> AE
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

interp :: String -> AE
interp = eval . parseAE
