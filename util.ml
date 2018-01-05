let space = String.contains " \t\n\r"
and punctuation = String.contains "(){}[],"
and symbolic = String.contains "~`!@#$%^&*-+=|\\:;<>.?/"
and numeric = String.contains "0123456789"
and alphanumeric = String.contains "abcdefghijklmnopqrstuvwxyz_'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"

let rec lex_while prop inp =
  match inp with
    c::cs when prop c.[0] ->
      let tok, rest = lex_while prop cs
      in c ^ tok, rest
  | _ -> "", inp

let map_fst f (fst, snd) = (f fst, snd)

let lex_prop char chars =
  let prop =
    if alphanumeric(char.[0]) then alphanumeric
    else if symbolic(char.[0]) then symbolic
    else fun c -> false
  in map_fst ((^) char) (lex_while prop chars)

let rec lex inp =
  match snd(lex_while space inp) with
    [] -> []
  | c::cs ->
      let tok, rest = lex_prop c cs
      in tok::lex rest

let rec explode string =
  let length_string = String.length string in
  if length_string == 0 then []
  else String.make 1 string.[0]::explode (String.sub string 1 (length_string - 1))

let make_parser parser inp =
  let exp, rest = parser (lex(explode inp)) in
  if rest = [] then exp else failwith "Unparsed input"

let rec parse_infix sym update op parse inp =
  let exp, rest = parse inp in
  if rest <> [] && List.hd rest = sym
  then parse_infix sym update (update op exp) parse (List.tl rest)
  else op exp, rest

let parse_left_infix sym op =
  parse_infix sym (fun f exp1 exp2 -> op(f exp1, exp2)) (fun x -> x)

let parse_right_infix sym op =
  parse_infix sym (fun f exp1 exp2 -> f(op(exp1, exp2))) (fun x -> x)

let parse_list sym =
  parse_infix sym (fun f exp1 exp2 -> (f exp1)@[exp2]) (fun x -> [x])

let nextin inp tok = inp <> [] && List.hd inp = tok

let parse_bracketed parse close inp =
  let ast, rest = parse inp in
  if nextin rest close then ast, List.tl rest
  else failwith "Close expected"
