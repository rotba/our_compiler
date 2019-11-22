

(*ONLY FOR TESTING*) (*INCLUDE  "pc.ml"*);;

# use "pc.ml";;
  
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
  
(*#################################ROTEM#####################################*)
module Tok_char: sig
  val tok_char : char list -> sexpr*char list
end
=struct
let pref = PC.caten (PC.char '#') (PC.char '\\');;
let vis_char = PC.range (Char.chr ((Char.code ' ')+1)) '\127';;
let nam_char =
  PC.disj_list [
      PC.pack (PC.word_ci "newline") (fun (s)-> Char.chr 10);
      PC.pack (PC.word_ci "nul") (fun (s) -> Char.chr 0) ;
      PC.pack (PC.word_ci "page") (fun (s) -> Char.chr 12);
      PC.pack (PC.word_ci "return") (fun (s) -> Char.chr 13);
      PC.pack (PC.word_ci "space") (fun (s) -> Char.chr 32);
      PC.pack (PC.word_ci "tab") (fun(s) -> Char.chr 9)
    ];;

let tok_char =
  let chain = PC.caten pref (PC.disj nam_char vis_char) in
  PC.pack chain (fun (p,c) -> Char c);;
  
end;; (* struct Tok_char *)

module Tok_string: sig
  val tok_string : char list -> sexpr*char list
end
=struct
let double_quote_nt = PC.char '\"';;
let backslash_nt = PC.char '\\';;  
let str_let_chr s =
  match (PC.nt_any s) with
  |('\\',_) -> raise PC.X_no_match
  |('\"',_) -> raise PC.X_no_match
  |(e,s)-> (e,s);;

let str_met_chr =
  let meta_chr_nt ch = PC.caten backslash_nt (PC.char_ci ch) in
  PC.disj_list [
      PC.pack (meta_chr_nt 'r') (fun _ -> Char.chr 13);
      PC.pack (meta_chr_nt 'n') (fun _ -> Char.chr 10);
      PC.pack (meta_chr_nt 't') (fun _ -> Char.chr 9);
      PC.pack (meta_chr_nt 'f') (fun _ -> Char.chr 12);
      PC.pack (PC.caten backslash_nt backslash_nt) (fun _ -> Char.chr 92);
      PC.pack (PC.caten backslash_nt double_quote_nt) (fun _ -> Char.chr 34)
      ;
    ];;

let str_chr = PC.disj str_let_chr str_met_chr;;
let tok_string =
  let chain  = PC.caten (PC.caten double_quote_nt (PC.star str_chr))double_quote_nt in
  PC.pack chain (fun ((q1,e),q2) -> String (list_to_string e));;
  
end;; (* struct Tok_string *)

(*#################################ROTEM#####################################*)
(*#################################ALON#####################################*)
let tok_bool  = 
    let nt = (PC.caten (PC.char '#') (PC.disj (PC.char_ci 'f') (PC.char_ci 't'))) in 
    PC.pack nt (fun (x) ->
    match x with
    | ('#', 'f') -> Bool false
    | ('#', 't') -> Bool true);;
(*#################################ALON#####################################*)


let nt_whitespaces =
  let nt = (PC.star (PC.const (fun (ch) -> ch<=' '))) in
  PC.pack nt (fun _ -> String "");;

let nt_semi_colon = PC.char ';';;

let nt_nl = PC.char (Char.chr 10);;

let nt_line_comment =
  let nt_content = (PC.star (PC.diff PC.nt_any nt_nl)) in
  let nt = PC.caten nt_semi_colon nt_content
  let chain_nl =  PC.caten nt nt_nl in
  let chain_eoi = PC.caten nt PC.nt_end_of_input in
  let nt  = PC.disj chain_nl chain_eoi in    
  PC.pack nt (fun _ -> String "");;

let make_paired nt_left nt_right nt =
  let nt = PC.caten nt_left nt in
  let nt = PC.pack nt (function (_, e) -> e) in
  let nt = PC.caten nt nt_right in
  let nt = PC.pack nt (function (e, _) -> e) in
  nt;;



let rec nt_comment s = (PC.disj nt_line_comment nt_sexpr_comment) s
and nt_sexpr_comment s = PC.nt_none s
and nt_skip s = (PC.disj nt_comment nt_whitespaces) s
and make_spaced nt = make_paired nt_skip nt_skip nt 
and nt_sexpr s =
  let all_rules =
    PC.disj_list
      [
        Tok_char.tok_char;
        Tok_string.tok_string;
        tok_bool
      ] in
  let chain = PC.caten all_rules PC.nt_end_of_input in
  let spaced = make_spaced chain in
  let packed = PC.pack spaced (fun (res, empty) -> res) in
  packed s;;

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
  let (res, empty) = nt_sexpr (string_to_list string) in
  res;;
 


let read_sexprs string = raise X_not_yet_implemented;;


  
  
end;; (* struct Reader *)
