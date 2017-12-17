open List
open String

type expression
  = Var of string
  | Const of int
  | Add of expression * expression
  | Mul of expression * expression

let ex1 = Add(Mul(Const 2, Var "x"), Var "")

let simplify1 expr =
  match expr with
    Add(Const(x), Const(y)) -> Const(x + y)
  | Add(Const(0), x) -> x
  | Add(x, Const(0)) -> x
  | Mul(Const(x), Const(y)) -> Const(x * y)
  | Mul(x, Const(0)) -> Const(0)
  | Mul(Const(0), x) -> Const(0)
  | Mul(Const(1), x) -> x
  | Mul(x, Const(1)) -> x
  | _ -> expr

let rec simplify expr =
  match expr with
    Add(x, y) -> simplify1(Add(simplify x, simplify y))
  | Mul(x, y) -> simplify1(Mul(simplify x, simplify y))
  | _ -> simplify1 expr

let ex2 = Add(Mul(Add(Mul(Const(0), Var("x")), Const(1)), Const(3)), Const(12))

let re2 = simplify(ex2)

let space = contains " \t\n\r"
and punctuation = contains "(){}[],"
and symbolic = contains "~`!@#$%^&*-+=|\\:;<>.?/"
and numeric = contains "0123456789"
and alphanumeric = contains "abcdefghijklmnopqrstuvwxyz_'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"

let rec lexwhile prop inp =
  match inp with
    c::cs when prop c ->
      let tok, rest = lexwhile prop cs
      in c^tok, rest
  | _ -> "", inp
