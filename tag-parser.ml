#use "reader.ml";;
open PC;;
type constant =
  | Sexpr of sexpr
  | Void

type expr =
  | Const of constant
  | Var of string
  | If of expr * expr * expr
  | Seq of expr list
  | Set of expr * expr
  | Def of expr * expr
  | Or of expr list
  | LambdaSimple of string list * expr
  | LambdaOpt of string list * string * expr
  | Applic of expr * (expr list);;

let rec expr_eq e1 e2 =
  match e1, e2 with
  | Const Void, Const Void -> true
  | Const(Sexpr s1), Const(Sexpr s2) -> sexpr_eq s1 s2
  | Var(v1), Var(v2) -> String.equal v1 v2
  | If(t1, th1, el1), If(t2, th2, el2) -> (expr_eq t1 t2) &&
                                            (expr_eq th1 th2) &&
                                              (expr_eq el1 el2)
  | (Seq(l1), Seq(l2)
    | Or(l1), Or(l2)) -> List.for_all2 expr_eq l1 l2
  | (Set(var1, val1), Set(var2, val2)
    | Def(var1, val1), Def(var2, val2)) -> (expr_eq var1 var2) &&
                                             (expr_eq val1 val2)
  | LambdaSimple(vars1, body1), LambdaSimple(vars2, body2) ->
     (List.for_all2 String.equal vars1 vars2) &&
       (expr_eq body1 body2)
  | LambdaOpt(vars1, var1, body1), LambdaOpt(vars2, var2, body2) ->
     (String.equal var1 var2) &&
       (List.for_all2 String.equal vars1 vars2) &&
         (expr_eq body1 body2)
  | Applic(e1, args1), Applic(e2, args2) ->
     (expr_eq e1 e2) &&
       (List.for_all2 expr_eq args1 args2)
  | _ -> false;;
	
                       
exception X_syntax_error;;

(* let nt_bool = nt_none;;
 * let nt_char = nt_none;;
 * let nt_string = nt_none;;
 * let nt_number =nt_none;; *)

let nt_self_eval= function
  | Number(x) -> (Const (Sexpr (Number x)))
  | String(x) -> (Const (Sexpr(String x)))
  | Bool(x) -> (Const (Sexpr(Bool x)))
  | Char(x) -> (Const (Sexpr(Char x)))
  | TagRef(x) -> (Const (Sexpr(TagRef x)))
  |_ -> raise X_no_match;;
  
let nt_not_self_eval=

  let exp_handler = 
    match y with
      | Pair (Symbol ("qoute"), Pair (a, Nil)) -> a
      | x -> x
  
  let nt = function
  | TaggedSexpr(x,y) -> (
    try (
      (nt_self_eval y)
      (Const (Sexpr(TaggedSexpr (x, y))))
    )
    match X_no_match -> (
      (Const (Sexpr(TaggedSexpr (x, (exp_handler y)))))
    )
  )
  |_ -> raise X_no_match;;


let nt_quoted = nt_none ;;

let nt_not_quoted =  
  disj_list [
      nt_self_eval;
      nt_not_self_eval;
    ];;

let nt_const =
    disj_list [
      nt_quoted;
      nt_not_quoted;
    ];;


let nt_expr s =
  let all_rules =
    disj_list
      [
        nt_const
      ] in
  all_rules s;;

module type TAG_PARSER = sig
  val tag_parse_expression : sexpr -> expr
  val tag_parse_expressions : sexpr list -> expr list
end;; (* signature TAG_PARSER *)

module Tag_Parser : TAG_PARSER = struct

let reserved_word_list =
  ["and"; "begin"; "cond"; "define"; "else";
   "if"; "lambda"; "let"; "let*"; "letrec"; "or";
   "quasiquote"; "quote"; "set!"; "unquote";
   "unquote-splicing"];;  

(* work on the tag parser starts here *)

let tag_parse_expression sexpr =
 nt_expr sexpr;;

let tag_parse_expressions sexpr = raise X_not_yet_implemented;;

  
end;; (* struct Tag_Parser *)
