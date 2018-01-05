#use "util.ml"

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
let rs2 = simplify(ex2)

let rec explodeC string =
  if length string == 0 then []
  else string.[0]::explodeC (sub string 1 (length string - 1))

let ex3 = "2 * ((var_1 + x') + 11)"
let rs3 = lex (explode ex3)
let ex4 = "if (*p1-- == *p2++) then f() else g()"
let rs3 = lex (explode ex4)

let rec parse_expression inp =
  match parse_product inp with
    exp1, "+"::rest ->
      let exp2, rest' = parse_expression rest
      in Add(exp1, exp2), rest'
  | exp1, rest -> exp1, rest
and parse_product inp =
  match parse_atom inp with
    exp1, "*"::rest ->
      let exp2, rest' = parse_expression rest
      in Mul(exp1, exp2), rest'
  | exp1, rest -> exp1, rest
and parse_atom inp =
  match lex inp with
    [] -> failwith "Expected an expression at end of input"
  | "("::rest ->
      (match parse_expression rest with
        exp, ")"::rest' -> exp, rest'
      | _ -> failwith "Expected closing parenthesis")
  | tok::rest ->
      if for_all numeric (explodeC tok) then Const(int_of_string tok), rest
      else Var(tok), rest

let default_parser = make_parser parse_expression

let ex4 = "x + 1"
let rs4 = default_parser ex4

let rec string_of_exp pr e =
  match e with
    Var s -> s
  | Const n -> string_of_int n
  | Add(x, y) ->
     let s = string_of_exp 2 x ^ " + " ^ string_of_exp 2 y
     in if pr > 2 then "(" ^ s ^ ")" else s
  | Mul(x, y) ->
     let s = string_of_exp 4 x ^ " * " ^ string_of_exp 4 y
     in if pr > 4 then "(" ^ s ^ ")" else s

let print_exp e = Format.print_string ("<<" ^ string_of_exp 0 e ^ ">>")
