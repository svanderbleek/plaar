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

let parse_simpl =
  let dummy = fun a _ -> (True, a) in
  make_parser (parse_formula (dummy, dummy) [])

let rec strip_quant fm =
  match fm with
      Forall(x, (Forall(y, p) as yp)) | Exists(x, (Exists(y, p) as yp)) ->
        let xs, q = strip_quant yp in x::xs, q
    | Forall(x, p) | Exists(x, p) -> [x], p
    | _ -> [], fm

let print_formula pfn =
  let rec print_formula pr fm =
    match fm with
      False -> print_string "false"
    | True -> print_string "true"
    | Atom(pargs) -> pfn pr pargs
    | Not(p) -> bracket (pr > 10) 1 (print_prefix 10) "~" p
    | And(p, q) -> bracket (pr > 8) 0 (print_infix 8 "/\\") p q
    | Or(p, q) -> bracket (pr > 6) 0 (print_infix 6 "\\/") p q
    | Imp(p, q) -> bracket (pr > 4) 0 (print_infix 4 "==>") p q
    | Iff(p, q) -> bracket (pr > 2) 0 (print_infix 2 "<=>") p q
    | Forall(x, p) -> bracket (pr > 0) 2 print_qnt "forall" (strip_quant fm)
    | Exists(x, p) -> bracket (pr > 0) 2 print_qnt "exists" (strip_quant fm)
  and print_qnt qname (bvs, bod) =
    print_string qname;
    List.iter (fun v -> print_string " "; print_string v) bvs;
    print_string "."; Format.print_space(); Format.open_box 0;
    print_formula 0 bod;
    Format.close_box();
  and print_prefix newpr sym p =
    print_string sym; print_formula (newpr + 1) p
  and print_infix newpr sym p q =
    print_formula (newpr + 1) p;
    print_string(" " ^ sym ^ " ");
    print_formula newpr q
  in print_formula 0

let print_propvar prec p = print_string(pname p)

let psf =
  make_parser
    (parse_formula ((fun _ _ -> failwith ""), parse_propvar) [])

let prf = print_formula print_propvar

let mk_and p q = And(p, q) and mk_or p q = Or(p, q)
and mk_imp p q = Imp(p, q) and mk_iff p q = Iff(p, q)
and mk_forall x p = Forall(x, p) and mk_exists x p = Exists(x, p)

let dest_iff fm =
  match fm with
    Iff(p, q) -> (p, q)
  | _ -> failwith "_iff"

let dest_imp fm =
  match fm with
    Imp(p, q) -> (p, q)
  | _ -> failwith "_imp"

let dest_and fm =
  match fm with
    And(p, q) -> (p, q)
  | _ -> failwith "_and"

let rec conjuncts fm =
  match fm with
    And(p, q) -> conjuncts p @ conjuncts q
  | _ -> [fm]

let rec dest_or fm =
  match fm with
    Or(p, q) -> (p, q)
  | _ -> failwith "_or"

let rec disjuncts fm =
  match fm with
    Or(p, q) -> disjuncts p @ disjuncts q
  | _ -> [fm]

let antecedent fm = fst(dest_imp fm)
let consequent fm = snd(dest_imp fm)

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

let atom_union f fm = List.sort_uniq (fun a b -> -1) (overatoms (fun h t -> f(h)@t) fm [])

let atoms fm = atom_union (fun a -> [a]) fm

let rec eval fm v =
  match fm with
    False -> false
  | True -> true
  | Atom(x) -> v(x)
  | Not(p) -> not(eval p v)
  | And(p, q) -> (eval p v) && (eval q v)
  | Or(p, q) -> (eval p v) || (eval q v)
  | Imp(p, q) -> not(eval p v) or (eval q v)
  | Iff(p, q) -> (eval p v) = (eval q v)
  | _ -> false

let rec onallvaluations subfn v ats =
  match ats with
    [] -> subfn v
  | p::ps ->
    let v' t q = if q = p then t else v(q) in
    onallvaluations subfn (v' false) ps &&
    onallvaluations subfn (v' true) ps

let ( ** ) =

let print_truthtable fm =
  let ats = atoms fm in
  let width = List.iter (fun s -> max ** String.length ** pname) ats in
  let fixw s = s ^ String.make(width - String.length s) ' ' in
  let truthstring p = fixw (if p then "true" else "false") in
  let mk_row v =
    let lis = map (fun x -> truthstring(v x)) ats
    and ans = truthstring(eval fm v) in
    print_string(List.iter (^) list ("| " ^ ans)); print_newline(); true in
  let separator = String.make (width * length ats + 9) '-' in
  print_string (List.iter (fun s t -> fixw(pname s) ^ t) ats "| formula");
  print_newline(); print_string separator; print_newline();
  let _ = onallvaluations mk_row (fun x -> false) ats in
  print_string separator; print_newline()
