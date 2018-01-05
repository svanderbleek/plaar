#use "util.ml"

type 'a formula
  = False
  | True
  | Atom of 'a
  | Not of 'a formula
  | And of 'a formula * 'a formula
  | Or of 'a formula * 'a formula
  | Imp of 'a formula * 'a formula
  | Iff of 'a formula * 'a formula
  | Forall of string * 'a formula
  | Exists of string * 'a formula

type prop = P of string

let mkp s = Atom(P(s))

let pname(P s) = s

let rec parse_atomic_formula (infix, arbitrary) xs inp =
  match inp with
    [] -> failwith "Formula expected"
  | "false"::rest -> False, rest
  | "true"::rest -> True, rest
  | "("::rest ->
    (try infix xs inp with
      Failure _ -> parse_bracketed (parse_formula (infix, arbitrary) xs) ")" rest)
  | "~"::rest ->
    map_fst (fun p -> Not p) (parse_atomic_formula (infix, arbitrary) xs rest)
  | "forall"::x::rest ->
    parse_quant (infix, arbitrary) (x::xs) (fun (x, p) -> Forall(x, p)) x rest
  | "exists"::x::rest ->
    parse_quant (infix, arbitrary) (x::xs) (fun (x, p) -> Exists(x, p)) x rest
  | _ -> arbitrary xs inp
and parse_quant (infix, arbitrary) xs quant x inp =
  match inp with
    [] -> failwith "Body of quant expected"
  | y::rest ->
    map_fst
      (fun fm -> quant(x, fm))
      (if y = "."
      then parse_formula (infix, arbitrary) xs rest
      else parse_quant (infix, arbitrary) (y::xs) quant y rest)
and parse_formula (infix, arbitrary) xs inp =
  parse_right_infix "<=>" (fun (p, q) -> Iff(p, q))
  (parse_right_infix "==>" (fun (p, q) -> Imp(p, q))
  (parse_right_infix "\\/" (fun (p, q) -> Or(p, q))
  (parse_right_infix "/\\" (fun (p, q) -> And(p, q))
  (parse_atomic_formula (infix, arbitrary) xs)))) inp

let parse_propvar vs inp =
  match inp with
    p::rest when p <> "(" -> Atom(P(p)), rest
  | _ -> failwith "parse_propvar"

let mk_and p q = And(p, q) and mk_or p q = Or(p, q)
and mk_imp p q = Imp(p, q) and mk_iff p q = Iff(p, q)
and mk_forall x p = Forall(x, p) and mk_exists x p = Exists(x, p)

let _iff fm =
  match fm with
    Iff(p, q) -> (p, q)
  | _ -> failwith "_iff"

let _imp fm =
  match fm with
    Imp(p, q) -> (p, q)
  | _ -> failwith "_imp"

let _and fm =
  match fm with
    And(p, q) -> (p, q)
  | _ -> failwith "_and"

let rec conjuncts fm =
  match fm with
    And(p, q) -> conjuncts p @ conjuncts q
  | _ -> [fm]

let rec _or fm =
  match fm with
    Or(p, q) -> (p, q)
  | _ -> failwith "_or"

let rec disjuncts fm =
  match fm with
    Or(p, q) -> disjuncts p @ disjuncts q
  | _ -> [fm]

let antecedent fm = fst(_imp fm)
let consequent fm = snd(_imp fm)

let rec onatoms f fm =
  match fm with
    Atom a -> f a
  | Not(p) -> Not(onatoms f p)
  | And(p, q) -> And(onatoms f p, onatoms f q)
  | Or(p, q) -> Or(onatoms f p, onatoms f q)
  | Imp(p, q) -> Imp(onatoms f p, onatoms f q)
  | Iff(p, q) -> Iff(onatoms f p, onatoms f q)
  | Forall(x, p) -> Forall(x, onatoms f p)
  | Exists(x, p) -> Exists(x, onatoms f p)
  | _ -> fm

let rec overatoms f fm b =
  match fm with
    Atom(a) -> f a b
  | And(p, q) | Or(p, q) | Imp(p, q) | Iff(p, q) -> overatoms f p (overatoms f q b)
  | Forall(x, p)  | Exists(x, p) -> overatoms f p b
  | _ -> b
