module Lexer where 

import Data.Char 

data Token = TokenNum Int 
           | TokenTrue 
           | TokenFalse
           | TokenPlus 
           | TokenTimes 
           | TokenMinus
           | TokenAnd 
           | TokenOr 
           | TokenXor
           | TokenLParen 
           | TokenRParen 
           | TokenVar String
           | TokenLambda
           | TokenArrow
           | TokenIf
           | TokenElse
           | TokenThen
           | TokenVirgula
           deriving Show 

data Expr = Num Int 
          | BTrue 
          | BFalse 
          | Add Expr Expr 
          | Sub Expr Expr
          | Times Expr Expr 
          | And Expr Expr 
          | Or Expr Expr 
          | Xor Expr Expr
          | Paren Expr 
          | If Expr Expr Expr 
          | Var String
          | Lam String Expr 
          | App Expr Expr 
          | Tuple [Expr]
          deriving Show 

data Ty = TNum 
        | TBool 
        | Tvar
        deriving (Show, Eq) 

lexer :: String -> [Token]
lexer [] = []
lexer ('+':cs) = TokenPlus : lexer cs 
lexer ('*':cs) = TokenTimes : lexer cs 
lexer ('(':cs) = TokenLParen : lexer cs 
lexer (')':cs) = TokenRParen : lexer cs
lexer (',':cs) = TokenVirgula : lexer cs 
lexer ('&':'&':cs) = TokenAnd : lexer cs 
lexer ('|':'|':cs) = TokenOr : lexer cs  
lexer ('\\':cs) = TokenLambda : lexer cs
lexer ('-':'>':cs) = TokenArrow : lexer cs
lexer ('-':cs) = TokenMinus : lexer cs 
lexer ('^':cs) = TokenXor : lexer cs 
lexer (c:cs)    | isSpace c = lexer cs 
                | isDigit c = lexNum (c:cs)
                | isAlpha c = lexKw (c:cs)
lexer _ = error "Lexical error"

lexNum cs = case span isDigit cs of 
            (num, rest) -> TokenNum (read num) : lexer rest 

lexKw cs = case span isAlpha cs of 
                ("true", rest) -> TokenTrue : lexer rest 
                ("false", rest) -> TokenFalse : lexer rest
                ("if", rest) -> TokenIf : lexer rest
                ("else", rest) -> TokenElse : lexer rest
                ("then", rest) -> TokenThen : lexer rest
                (var, rest) -> TokenVar var : lexer rest 