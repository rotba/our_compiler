(* ONLY FOR TESTING *)
(* INCLUDE "pc.ml";; *)

# use "pc.ml";;
  
open Format;;

exception X_not_yet_implemented;;
exception X_this_should_not_happen;;
exception Exhausting;;
let seq_to_string params to_string=
  let rec aggregate = function
  |[] -> []
  |f::r -> (to_string f) :: (aggregate r) in
  let params = (aggregate params) in
  String.concat " , " params;;

let seq'_to_string params to_string=
  let rec aggregate = function
  |[] -> []
  |f::r -> (to_string f) :: (aggregate r) in
  let params = (aggregate params) in
  String.concat " , " params;;

let string_list_to_string params=
  let rec aggregate = function
  |[] -> []
  |f::r -> f :: (aggregate r) in
  let params = (aggregate params) in
  String.concat " , " params;;

  
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

let rec sexpr_to_string  =
  function
  |Char(x) -> Printf.sprintf "Char(%c)" x
  |String(x) -> x
  |Nil-> "Nil"
  |Symbol(x)-> Printf.sprintf "Symbol(%s)" x
  |Number(Float(x))-> string_of_float x
  |Number(Int(x))-> string_of_int x                           
  |Pair(x,y) -> String.concat "" ["Pair( "; (sexpr_to_string x); " , "; (sexpr_to_string y) ;" )"]
  |TaggedSexpr(s,e) -> String.concat " , "["TaggedSexpr("; s;  (sexpr_to_string e) ;")"]
  |TagRef(x)->String.concat " , "["TagRef("; x;")"]
  |Bool(false)-> "false"
  |Bool(true) -> "true";;

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


let make_paired nt_left nt_right nt =
  let nt = PC.caten nt_left nt in
  let nt = PC.pack nt (function (_, e) -> e) in
  let nt = PC.caten nt nt_right in
  let nt = PC.pack nt (function (e, _) -> e) in
  nt;;
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

let rec list_packer = function
  | [] -> Nil
  | e::s -> Pair(e, (list_packer s));;

  
let qoute_forms_const name sexp = Pair(Symbol(name),Pair(sexp, Nil));;
let qoute_forms_packer =
  function
  |("\'",s) -> qoute_forms_const "quote" s
  |(",",s) -> qoute_forms_const "unquote" s
  |("`",s) -> qoute_forms_const "quasiquote" s
  |((",@"),s) -> qoute_forms_const "unquote-splicing" s
  | _ -> raise Exhausting
  ;;


let rec tagged_again s e =
    match e with
    |TaggedSexpr(name, sexpr) ->
      (name = s) || (tagged_again s sexpr)
    |Pair(a,b)->
      (tagged_again s a) || (tagged_again s b)
    | _ -> false ;;

let tagged_packer =
  function  
  |(Symbol(s), None) -> TagRef(s)
  |(Symbol(s), Some(sexpr)) ->
    (
      if(tagged_again s sexpr)
      then raise X_this_should_not_happen
      else TaggedSexpr(s,sexpr)
    )
  | _ -> raise Exhausting
    ;;

let rec get_tags =
  function
  |Pair(x,y) -> List.append (get_tags x) (get_tags y)
  |TaggedSexpr(s,sexpr) -> List.append [s] (get_tags sexpr)
  |_->[];;

let disjoint l1 l2 =
  let contains x= List.exists (fun (y) -> x=y) l2 in
  let inter = List.filter (contains) l1 in
  (inter) == [];;

  (* TODO: add test for x *)
let rec is_unique_tags = 
   function  
   |Pair(x,y) ->
     let x_tags = get_tags x in
     let y_tags = get_tags y in
     (disjoint x_tags y_tags) && (is_unique_tags y)
   |_ ->true;;

let nt_unique_tags nt s=
  let (s1,e) = nt s in
  if(is_unique_tags s1) then (nt s) else ((fun _ -> raise X_this_should_not_happen) s);;
  


(*#################################ROTEM#####################################*)
(*#################################ALON#####################################*)
let tok_bool  = 
    let nt = (PC.caten (PC.char '#') (PC.disj (PC.char_ci 'f') (PC.char_ci 't'))) in 
    PC.pack nt (fun (x) ->
    match x with
      | ('#', 'f') -> Bool false
      | ('#', 'F') -> Bool false
      | ('#', 'T') -> Bool true
      | ('#', 't') -> Bool true
      | _ -> raise Exhausting)
      ;;

let nt_digit = PC.range '0' '9';;
let nt_natural = PC.plus nt_digit;;

let nt_letter_ci = PC.range_ci 'a' 'z';;
let nt_Punc = PC.one_of "!$^*-_=+<>/?:";;
let nt_sym_char = PC.disj_list [
  nt_letter_ci;
  nt_digit;
  nt_Punc
];;

let nt_sym = PC.plus nt_sym_char;;
let tok_sym = 
  PC.pack nt_sym (fun (x) ->
  Symbol (String.lowercase_ascii (list_to_string x)));;



let nt_radix_digit base = 
  if (base <= 0 || 36 < base)
  then raise PC.X_no_match
  else (
    if (base <= 10)
    then (PC.range '0' (char_of_int ((Char.code '0') + (base - 1))))
    else ( PC.disj
            (PC.range '0' '9')
            (PC.range_ci 'a' (char_of_int ((Char.code 'a') + ((base - 10) - 1))))
    )
  );;
let nt_sign = (PC.maybe (PC.disj (PC.char '+') (PC.char '-')));;
let nt_int_gen nt_digis = (PC.caten nt_sign nt_digis);;
let nt_int = 
  PC.not_followed_by (nt_int_gen nt_natural) (PC.disj nt_letter_ci nt_Punc);;
let nt_int_for_science = nt_int_gen nt_natural;;

(* let nt_int = (PC.caten nt_sign nt_natural);; *)
let tok_int = 
  PC.pack nt_int ( fun (x) ->
  match x with
    | (None, e) -> Int (int_of_string (list_to_string e))
    | (Some(s), e) -> Int (int_of_string (list_to_string (s::e))));;

let nt_float_gen nt_digis = (PC.caten nt_int (PC.caten (PC.char '.') nt_digis));;
let nt_float = nt_float_gen nt_natural;;
(* let nt_float = (PC.caten nt_int (PC.caten (PC.char '.') nt_natural));; *)
let tok_float = 
  PC.pack nt_float ( fun (x) ->
  match x with
    | ((None, e), (d,m)) ->  Number (Float (float_of_string (list_to_string (List.append e (d::m)))))
    | ((Some(s), e), (d,m)) -> Number (Float (float_of_string (list_to_string (List.append (s::e) (d::m))))));;

let tok_num_gen nt_int tok_float s = 
  try (PC.pack (PC.not_followed_by nt_int (PC.char '.')) ( fun (x) ->
  match x with
    | (None, e) -> Number (Int (int_of_string (list_to_string e)))
    | (Some(s), e) -> Number (Int (int_of_string (list_to_string (s::e))))) s)
  with PC.X_no_match -> tok_float s;;

let tok_num s = (tok_num_gen nt_int tok_float) s;; 

let char_digit_to_value char = 
  if ('0' <= char && char <= '9')
  then ((Char.code char) - (Char.code '0'))
  else ((Char.code (Char.lowercase_ascii char)) - (Char.code 'a') + 10)

let calc_no_mant b s =
  let s = List.map char_digit_to_value s in
  let agg acc curr = curr + b*acc in
  List.fold_left agg 0 s;;

let calc_mant base f =
  let f = List.map char_digit_to_value f in
  let f = List.map float_of_int f in
  let agg curr acc = (curr +.acc)*. (1./.base) in
  List.fold_right agg f 0.;;

let dig_list_to_float base n f =
  let no_mant = (float_of_int (calc_no_mant base n)) in
  let mant = calc_mant (float_of_int base) f in
  no_mant +. mant;;


let calc_sign = function
|'+' -> 1
|'-' -> -1
| _ -> raise Exhausting
;;

let char_list_to_int s = int_of_string (list_to_string s);;

let tok_radix s =
  let nt = make_paired (PC.char '#') (PC.char_ci 'r') nt_natural in 
  let (base, s) = nt s in
  let base = char_list_to_int base in
  let nt_radix_int = (PC.not_followed_by (nt_int_gen (PC.plus (nt_radix_digit base))) (PC.char '.')) in
  try (PC.pack nt_radix_int ( fun (x) ->
  match x with
    | (None, e) -> Number (Int (calc_no_mant base e))
    | (Some(s), e) -> Number (Int ( (calc_sign s)*(calc_no_mant base e)) )) s)
  with PC.X_no_match -> (
  let nt_radix_float = (nt_float_gen (PC.plus (nt_radix_digit base))) in
  PC.pack nt_radix_float ( fun (x) ->
  match x with
    | ((None, e), (d,m)) ->  Number (Float (dig_list_to_float base e m))
    | ((Some(s), e), (d,m)) -> Number (Float ((float_of_int (calc_sign s))*.(dig_list_to_float base e m)))) s);;
  


let rec dotted_list_list_packer = function
  | (e::[], (d, s)) -> Pair(e, s)
  | (e::s, f) -> Pair(e, (dotted_list_list_packer (s, f)))
  |([],_)-> raise Exhausting
;;

let nt_float_to_string = 
  PC.pack nt_float ( fun (x) ->
  match x with
    | ((None, e), (d,m)) ->  list_to_string (List.append e (d::m))
    | ((Some(s), e), (d,m)) -> list_to_string (List.append (s::e) (d::m)));;


let nt_int_to_string s = 
  PC.pack nt_int_for_science ( fun (x) ->
  match x with
    | (None, e) -> (list_to_string e)
    | (Some(s), e) -> (list_to_string (s::e))) s;;


let nt_num_to_string s = 
try (PC.pack (PC.not_followed_by nt_int_for_science (PC.char '.')) ( fun (x) ->
match x with
  | (None, e) -> (list_to_string e)
  | (Some(s), e) -> (list_to_string (s::e))) s)
with PC.X_no_match -> nt_float_to_string s;;


let nt_scientific =
  PC.caten nt_num_to_string (PC.caten (PC.char_ci 'e') nt_int_to_string);;

let tok_scientific = 
  PC.pack nt_scientific ( fun (x) ->
  match x with
    | (man, (e, exp)) -> (Number (Float (float_of_string (man ^ "e" ^ exp))))
    );;






(*#################################ALON#####################################*)


let nt_whitespaces =
  let nt = PC.const (fun (ch) -> ch<=' ') in
  PC.pack nt (fun _ -> String "");;
let nt_semi_colon = PC.char ';';;
let nt_lparen = PC.char '(';;
let nt_rparen = PC.char ')';;
let nt_semi_colon = PC.char ';';;
let nt_nl = PC.char (Char.chr 10);;
let nt_qoute_pref = PC.pack (PC.char '\'') (fun _ -> "\'");;
let nt_qqoute_pref = PC.pack (PC.char '`') (fun _ -> "`");;
let nt_unqoute_pref = PC.pack (PC.char ',') (fun _ -> ",");;
let nt_unqoute_splicing_pref = PC.pack (PC.word ",@") (fun _ -> ",@");;
let nt_line_comment =
  let nt_content = (PC.star (PC.diff PC.nt_any nt_nl)) in
  let pref = PC.caten nt_semi_colon nt_content in
  let chain_nl =  PC.caten pref nt_nl in
  let chain_eoi = PC.pack (PC.caten pref PC.nt_end_of_input) (fun (res,empty1)->(res, '\n'))in
  let nt  = PC.disj chain_nl chain_eoi in    
  PC.pack nt (fun _ -> String "");;


let nt_print_and_fail char_list  = 
  let () =
    (Printf.printf ";;; Reader failed :\n%s\n"
     (list_to_string char_list)
     )in
     raise PC.X_no_match
      ;;


let rec nt_comment s = (PC.disj nt_line_comment nt_sexpr_comment) s
and nt_sexpr_comment s = (PC.pack (PC.caten (PC.word "#;") nt_sexpr) (function _ -> String "") ) s
and nt_skip s = (PC.disj nt_comment nt_whitespaces) s
and make_spaced nt = make_paired (PC.star nt_skip) (PC.star nt_skip) nt
and nt_list s =
  let nt = make_paired (PC.caten nt_lparen (PC.star nt_skip)) (PC.caten (PC.star nt_skip) nt_rparen) (PC.star nt_sexpr) in
  let nt = PC.pack nt list_packer in
  nt s
and nt_dotted_list s =
  let nt = PC.caten (PC.plus nt_sexpr) (PC.caten (PC.char '.') nt_sexpr) in 
  let nt = make_paired (PC.caten nt_lparen (PC.star nt_skip)) (PC.caten (PC.star nt_skip) nt_rparen) nt in
  let nt = PC.pack nt dotted_list_list_packer in
  nt s
and nt_qoute s = 
  let nt = PC.caten nt_qoute_pref nt_sexpr in
  let nt = PC.pack nt qoute_forms_packer in
  nt s
and nt_qqoute s = 
  let nt = PC.caten nt_qqoute_pref nt_sexpr in
  let nt = PC.pack nt qoute_forms_packer in
  nt s
and nt_unqoute s = 
  let nt = PC.caten nt_unqoute_pref nt_sexpr in
  let nt = PC.pack nt qoute_forms_packer in
  nt s
and nt_unqoute_splicing s = 
  let nt = PC.caten nt_unqoute_splicing_pref nt_sexpr in
  let nt = PC.pack nt qoute_forms_packer in
  nt s
and nt_tagged s =
  let nt_tag = make_paired (PC.word "#{") (PC.char '}') tok_sym in
  let nt_eq_sexpr = PC.caten (PC.char '=') nt_sexpr in
  let nt_eq_sexpr = PC.pack nt_eq_sexpr (fun (_,e)->e) in
  let nt = PC.caten nt_tag (PC.maybe nt_eq_sexpr) in
  let nt = PC.pack nt tagged_packer in
  nt s
and nt_sexpr s =
  let all_rules =
    PC.disj_list
      [
        tok_bool;  
        Tok_char.tok_char;
        tok_scientific;
        tok_radix;
        tok_num;
        Tok_string.tok_string;
        tok_sym;
        nt_list;
        nt_dotted_list;
        nt_qoute;
        nt_qqoute;
        nt_unqoute;
        nt_unqoute_splicing;
        nt_tagged;
      ] in
  let spaced = make_spaced all_rules in
  spaced s;;

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
  let nt =  nt_unique_tags nt_sexpr in
  let nt = PC.caten nt PC.nt_end_of_input in
  let ((res,empty1), empty2) = nt (string_to_list string) in
  res;;



let read_sexprs string = 
  let nt = PC.caten (PC.star nt_sexpr) (PC.caten (PC.star nt_skip) PC.nt_end_of_input) in
  let ((res,empty1), empty2) = (nt (string_to_list string))in
   res;;


  
  
end;; (* struct Reader *)
