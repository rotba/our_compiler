#use "reader.ml";;
open PC;;

exception Exhausting;;

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

let handle_tagged first sec =
  let rec sec_handler = function
    | Pair (Symbol ("quote"), Pair (a, Nil)) -> a
    | Pair(a,b) -> Pair((sec_handler a),(sec_handler b))
    | x-> x in
  let sec = sec_handler sec in
  Const(Sexpr(TaggedSexpr(first,sec)));;

let rec is_proper = function
  |Pair(_,Nil)-> true
  |Pair(car,cdr)-> (is_proper cdr)
  |_->false;;




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

let rec tag_parse_expression sexpr =
  match sexpr with
  | Pair(Symbol("quasiquote"),cdr) -> handle_qq(cdr)
  | Number(x) -> (Const (Sexpr (Number x)))
  | String(x) -> (Const (Sexpr(String x)))
  | Bool(x) -> (Const (Sexpr(Bool x)))
  | Char(x) -> (Const (Sexpr(Char x)))
  | TagRef(x) -> (Const (Sexpr(TagRef x)))
  | TaggedSexpr(first, sec) ->(handle_tagged first sec)
  | Pair(Symbol("quote"), Pair(sec,Nil)) ->Const(Sexpr(sec))
  | Symbol(s) ->if (List.exists (fun(e)-> e=s) reserved_word_list) then raise X_syntax_error else (Var(s))
  | Pair(Symbol "if", Pair(a, Pair(b, Pair(c, Nil)))) -> If ((tag_parse_expression (a)), (tag_parse_expression (b)), (tag_parse_expression (c)))
  | Pair(Symbol "if", Pair(a, Pair(b, Nil))) -> If ((tag_parse_expression (a)), (tag_parse_expression (b)), Const(Void))
  | Pair(Symbol("lambda"), cdr) ->(handle_lambda cdr)
  | Pair(Symbol("or"), cdr) ->Or((handle_or cdr))
  | Pair(Symbol("set!"), Pair(name, Pair(value,Nil))) ->Set(tag_parse_expression(name),tag_parse_expression(value) )
  | Pair(Symbol "define", Pair(Symbol(a), Pair(b, Nil))) -> Def ((tag_parse_expression (Symbol(a))), (tag_parse_expression (b)))
  | Pair(Symbol "begin", a) -> (handle_begin a)
  (*################################################################################# *)
  | Pair(a, b) -> Applic ((tag_parse_expression a), (parse_applic_body b))              
  |_ -> raise X_syntax_error


  
and parse_applic_body = function
  | Nil -> []
  | Pair(a, b) -> tag_parse_expression(a)::parse_applic_body(b)
  | _ -> raise Exhausting  

and handle_begin = function
  |Nil -> Const(Void)
  |Pair(car, Nil) -> (tag_parse_expression  car)
  |Pair(car, cdr) ->(Seq(parse_applic_body (Pair(car, cdr))))
  | _->raise X_syntax_error


  
and handle_lambda cdr =
  let rec to_list = function
  |Nil-> []
  |Pair(Symbol(v),cdr)-> (List.append [v] (to_list cdr))
  | _ -> raise Exhausting in

  let rec handle_body = function
    |Pair(car, Nil) -> (tag_parse_expression  car)
    |Pair(car, cdr) ->(Seq(parse_applic_body (Pair(car, cdr))))
    | _->raise X_syntax_error  in

  let rec get_opt_list = function
    |Pair(Symbol(vn),Symbol(vs))->([vn], vs)
    |Pair(Symbol(vn_m_1), cdr) ->
      let (acc, vs) = (get_opt_list cdr) in
      (vn_m_1::acc,vs)
    |_ -> raise X_syntax_error in
       
  match cdr with
  |Pair(params, body)->
    if((is_proper params))
    then LambdaSimple((to_list params), (handle_body body))
    else
      let (params, opt) = (get_opt_list params) in
      LambdaOpt(params, opt, (handle_body body))
  |_ -> raise X_syntax_error
      
and handle_or cdr =
match cdr with
| Nil -> []
| Pair(car, cdr) -> tag_parse_expression(car)::handle_or(cdr)  
| _ -> raise Exhausting
     
and handle_qq = function
  |
;;

let tag_parse_expressions sexpr = 
  List.map tag_parse_expression sexpr;;

  
end;; (* struct Tag_Parser *)
