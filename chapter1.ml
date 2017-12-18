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

let space = contains " \t\n\r"
and punctuation = contains "(){}[],"
and symbolic = contains "~`!@#$%^&*-+=|\\:;<>.?/"
and numeric = contains "0123456789"
and alphanumeric = contains "abcdefghijklmnopqrstuvwxyz_'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"

let rec explode string =
  if length string == 0 then []
  else make 1 string.[0]::explode (sub string 1 (length string - 1))

let rec explodeC string =
  if length string == 0 then []
  else string.[0]::explodeC (sub string 1 (length string - 1))

let rec lexwhile prop inp =
  match inp with
    c::cs when prop c.[0] ->
      let tok, rest = lexwhile prop cs
      in c ^ tok, rest
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

let ex3 = "2 * ((var_1 + x') + 11)"
let rs3 = lex (explode ex3)
let ex4 = "if (*p1-- == *p2++) then f() else g()"
let rs3 = lex (explode ex4)

let rec parse_expression i =
  match parse_product i with
    e1, "+"::i1 ->
      let e2, i2 = parse_expression i1
      in Add(e1, e2), i2
  | e1, i1 -> e1, i1
and parse_product i =
  match parse_atom i with
    e1, "*"::i1 ->
      let e2, i2 = parse_expression i1
      in Mul(e1, e2), i2
  | e1, i1 -> e1, i1
and parse_atom i =
  match lex i with
    [] -> failwith "Expected an expression at end of input"
  | "("::i1 ->
      (match parse_expression i1 with
        e2, ")"::i2 -> e2, i2
      | _ -> failwith "Expected closing parenthesis")
  | tok::i1 ->
      if for_all numeric (explodeC tok) then Const(int_of_string tok), i1
      else Var(tok), i1

let make_parser parser i =
  let e, rest = parser (lex(explode i))
  in if rest = [] then e else failwith "Unparsed input"

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
