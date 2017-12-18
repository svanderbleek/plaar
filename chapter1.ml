open List
open String
open Char

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

let rec explode string =
  if length string == 0 then []
  else escaped string.[0]::explode (sub string 1 (length string - 1))

let rec lexwhile prop inp =
  match inp with
    c::cs when prop c.[0] ->
      let tok, rest = lexwhile prop cs
      in c^tok, rest
  | _ -> "", inp

let mapfst f (fst, snd) =
  f fst, snd

let lexprop char chars =
  let prop =
    if alphanumeric(char.[0]) then alphanumeric
    else if symbolic(char.[0]) then symbolic
    else fun c -> false
  in mapfst ((^) char) (lexwhile prop chars)

let rec lex inp =
  match snd(lexwhile space inp) with
    [] -> []
  | c::cs ->
    let tok, rest = lexprop c cs
    in tok::lex rest
