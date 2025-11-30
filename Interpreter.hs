module Interpreter where 

import Lexer 
import Parser 

isValue :: Expr -> Bool 
isValue BTrue  = True 
isValue BFalse = True 
isValue (Num _) = True 
isValue (Lam _ _ _) = True 
isValue (Tuple xs) = all isValue xs
isValue _ = False 

subst :: String -> Expr -> Expr -> Expr 
subst x s y@(Var v) = if x == v then s else y 
subst x s (Num n) = (Num n)
subst x s BTrue = BTrue 
subst x s BFalse = BFalse 
subst x s (Lam y z t1) = Lam y z (subst x s t1)
subst x s (App t1 t2) = App (subst x s t1) (subst x s t2) 
subst x s (Add t1 t2) = Add (subst x s t1) (subst x s t2) 
subst x s (And t1 t2) = And (subst x s t1) (subst x s t2) 
subst x s (Times t1 t2) = Times (subst x s t1) (subst x s t2)
subst x s (Paren t) = Paren (subst x s t) 
-- Completar subst para outros termos da linguagem

step :: Expr -> Expr 
step (Add (Num n1) (Num n2)) = Num (n1 + n2)
step (Add (Num n1) e2) = let e2' = step e2
                           in Add (Num n1) e2' 
step (Add e1 e2) = Add (step e1) e2 

step (Times (Num n1) (Num n2)) = Num (n1 * n2)
step (Times (Num n1) e2) = let e2' = step e2
                            in Times (Num n1) e2'
step (Times e1 e2) = Times (step e1) e2

step (Sub (Num n1) (Num n2)) = Num (n1 - n2)
step (Sub (Num n1) e2) = let e2' = step e2
                           in Sub (Num n1) e2' 
step (Sub e1 e2) = Sub (step e1) e2 

step (And BFalse e2) = BFalse 
step (And BTrue e2) = e2 
step (And e1 e2) = And (step e1) e2 

step (Or BTrue e2) = BTrue 
step (Or BFalse e2) = e2 
step (Or e1 e2) = Or (step e1) e2 

step (Xor BTrue BFalse)  = BTrue
step (Xor BFalse BTrue)  = BTrue
step (Xor BFalse BFalse) = BFalse
step (Xor BTrue BTrue)   = BFalse

step (Xor e1 e2) = 
    if isValue e1 then
        Xor e1 (step e2)
    else 
        Xor (step e1) e2

step (App l@(Lam x _ e1) e2) = if (isValue e2) then 
                             subst x e2 e1 
                           else 
                             App l (step e2)

step (App e1 e2) = App (step e1) e2

step (If BTrue e2 e3) = e2
step (If BFalse e2 e3) = e3
step (If e1 e2 e3) = If (step e1) e2 e3

step (Paren p) = p

step (Tuple e)
    = case break (not . isValue) e of
        (_, []) -> Tuple e
        (y, x:xs) -> Tuple (y ++ [step x] ++ xs) 

step _ = error "fala baixo negue"

eval :: Expr -> Expr
eval e = if isValue e then e else eval (step e)