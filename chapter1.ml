type expression
  = Var of string
  | Const of int
  | Add of expression * expression
  | Mul of expression * expression

let ex1 = Add(Mul(Const 2, Var "x"), Var "")
