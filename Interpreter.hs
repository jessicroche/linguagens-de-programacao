import Lexer
import Parser

subst :: Expr -> Id -> Expr -> Expr
subst(Var y) x v
          | x == y      = v
          | otherwise = Var y
subst(App e1 e2) x v = App (subst e1 x v) (subst e2 x v)
subst (Lam y t body) x v 
          | x == y      = Lam y t body
          | otherwise = Lam y t (subst body x v)
subst (Sum e1 e2) x v = Sum(subst e1 x v ) (subst e2 x v)
subst (Mult e1 e2) x v = Mult(subst e1 x v) (subst e2 x v)
subst (Sub e1 e2) x v = Sub(subst e1 x v ) (subst e2 x v)
subst (Div e1 e2) x v = Div(subst e1 x v ) (subst e2 x v)
subst (Equal e1 e2 ) x v = Equal (subst e1 x v ) (subst e2 x v)
subst (If e1 e2 e3) x v = If(subst e1 x v) (subst e2 x v) (subst e3 x v)
subst (Diff e1 e2 ) x v = Diff (subst e1 x v ) (subst e2 x v)
subst (Maior e1 e2 ) x v = Maior (subst e1 x v ) (subst e2 x v)
subst (MaiorIgual e1 e2 ) x v = MaiorIgual (subst e1 x v ) (subst e2 x v)
subst (Menor e1 e2 ) x v = Menor (subst e1 x v ) (subst e2 x v)
subst (MenorIgual e1 e2 ) x v = MenorIgual (subst e1 x v ) (subst e2 x v)
subst (Not e1) x v = Not(subst e1 x v )
subst (Or e1 e2 ) x v = Or (subst e1 x v ) (subst e2 x v)
subst (And e1 e2 ) x v = And (subst e1 x v ) (subst e2 x v)

subst e _ _ = e

eval :: Expr -> Int 
eval (Num n) = n 
eval (Sum e1 e2) = 
    let v1 = eval e1 
        v2 = eval e2 
      in v1 + v2 
eval (Mult e1 e2) = 
    let v1 = eval e1 
        v2 = eval e2 
      in v1 * v2 

eval (Sub e1 e2) = 
    let v1 = eval e1 
        v2 = eval e2 
      in v1 - v2 

-- STEPS
step :: Expr -> Expr

--SOMA
step(Sum(Num n1) (Num n2)) = Num (n1+n2)
step(Sum (Num n1) e2) = 
    let e2' = step e2
        in Sum(Num n1) e2'
step(Sum e1 e2) = 
    let e1' = step e1
        in Sum e1' e2
--MULT
step(Mult(Num n1) (Num n2)) = Num (n1*n2)
step(Mult (Num n1) e2) = 
    let e2' = step e2
        in Mult(Num n1) e2'
step(Mult e1 e2) = 
    let e1' = step e1
        in Mult e1' e2

-- SUB
step(Sub(Num n1) (Num n2)) = Num (n1-n2)
step(Sub (Num n1) e2) = 
    let e2' = step e2
        in Sub(Num n1) e2'
step(Sub e1 e2) = 
    let e1' = step e1
        in Sub e1' e2

-- DIV
step(Div(Num n1) (Num n2)) = Num (n1 `div` n2)
step(Div (Num n1) e2) = 
    let e2' = step e2
        in Div(Num n1) e2'
step(Div e1 e2) = 
    let e1' = step e1
        in Div e1' e2

-- EQUAL
step(Equal(Num n1) (Num n2)) = Boolean (n1 == n2)
step(Equal (Num n1) e2) = 
    let e2' = step e2
        in Equal(Num n1) e2'
step(Equal e1 e2) = 
    let e1' = step e1
        in Equal e1' e2

-- DIFF
step(Diff(Num n1) (Num n2)) = Boolean (n1 /= n2)
step(Diff (Num n1) e2) = 
    let e2' = step e2
        in Diff(Num n1) e2'
step(Diff e1 e2) = 
    let e1' = step e1
        in Diff e1' e2

-- MAIOR
step(Maior(Num n1) (Num n2)) = Boolean (n1 > n2)
step(Maior (Num n1) e2) = 
    let e2' = step e2
        in Maior(Num n1) e2'
step(Maior e1 e2) = 
    let e1' = step e1
        in Maior e1' e2

-- MENOR
step(Menor(Num n1) (Num n2)) = Boolean (n1 < n2)
step(Menor (Num n1) e2) = 
    let e2' = step e2
        in Menor(Num n1) e2'
step(Menor e1 e2) = 
    let e1' = step e1
        in Menor e1' e2

-- MAIOR OU IGUAL
step(MaiorIgual(Num n1) (Num n2)) = Boolean (n1 >= n2)
step(MaiorIgual (Num n1) e2) = 
    let e2' = step e2
        in MaiorIgual(Num n1) e2'
step(MaiorIgual e1 e2) = 
    let e1' = step e1
        in MaiorIgual e1' e2

-- MENOR OU IGUAL
step(MenorIgual(Num n1) (Num n2)) = Boolean (n1 <= n2)
step(MenorIgual (Num n1) e2) = 
    let e2' = step e2
        in MenorIgual(Num n1) e2'
step(MenorIgual e1 e2) = 
    let e1' = step e1
        in MenorIgual e1' e2

step (App (Lam x t body) arg) = subst body x arg
step (App e1 e2) = App (step e1) e2

step e = e

execute :: Expr -> [Expr]
execute e | step e == e = [e]
          | otherwise = e : execute (step e)
-- step $ subst( Sum (Var "x") (Num 2) "x") (Num 10)