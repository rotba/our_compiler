

INCLUDE  "pc.ml";;
(*# use "pc.ml";;*)
open Format;;

exception X_not_yet_implemented;;
exception X_this_should_not_happen;;
  
type number =
  | Int of int
  | Float of float;;
  
type sexpr =
  | Bool of bool
  | Nil
  | Number of number
  | Char of char
  | String of string
  | Symbol of string
  | Pair of sexpr * sexpr
  | TaggedSexpr of string * sexpr
  | TagRef of string;;

let rec sexpr_eq s1 s2 =
  match s1, s2 with
  | Bool(b1), Bool(b2) -> b1 = b2
  | Nil, Nil -> true
  | Number(Float f1), Number(Float f2) -> abs_float(f1 -. f2) < 0.001
  | Number(Int n1), Number(Int n2) -> n1 = n2
  | Char(c1), Char(c2) -> c1 = c2
  | String(s1), String(s2) -> s1 = s2
  | Symbol(s1), Symbol(s2) -> s1 = s2
  | Pair(car1, cdr1), Pair(car2, cdr2) -> (sexpr_eq car1 car2) && (sexpr_eq cdr1 cdr2)
  | TaggedSexpr(name1, expr1), TaggedSexpr(name2, expr2) -> (name1 = name2) && (sexpr_eq expr1 expr2) 
  | TagRef(name1), TagRef(name2) -> name1 = name2
  | _ -> false;;
  

module Tok_char: sig
  val tok_char : char list -> sexpr*char list
end
=struct
let str_to_chr str =
    match str with
    |"newline" -> Char.chr 10
    |"nul" ->  Char.chr 0
    |"page" ->  Char.chr 12
    |"return" ->  Char.chr 13
    |"space" ->  Char.chr 32
    |"tab" ->  Char.chr 9
    |_ -> raise X_this_should_not_happen;;
let pref = PC.caten (PC.char '#') (PC.char '\\');;
let vis_char = PC.range (Char.chr ((Char.code ' ')+1)) '\127';;
let nam_char =
  let nt =
    PC.disj_list [
        PC.word "newline";
        PC.word "nul";
        PC.word "page";
        PC.word "return";
        PC.word "space";
        PC.word "tab"
      ] in
  PC.pack nt (fun (c) -> str_to_chr (list_to_string c));;

let tok_char =
  let chain = PC.caten pref (PC.disj nam_char vis_char) in
  PC.pack chain (fun (p,c) -> Char c);;
  
end;; (* struct Tok_char *)  

module Reader: sig
  val read_sexpr : string -> sexpr
  val read_sexprs : string -> sexpr list
end
= struct
let normalize_scheme_symbol str =

  let s = string_to_list str in
  if (andmap
	(fun ch -> (ch = (lowercase_ascii ch)))
	s) then str
  else Printf.sprintf "|%s|" str;;

let read_sexpr string =
  let all_rules =
    PC.disj_list
          [
            Tok_char.tok_char
          ] in
  let chain = PC.caten all_rules PC.nt_end_of_input in
  let ((res,empty), also_empty) = chain (string_to_list string) in
  res;;
 


let read_sexprs string = raise X_not_yet_implemented;;


  
  
end;; (* struct Reader *)
