#use "tag-parser.ml";;

type var = 
  | VarFree of string
  | VarParam of string * int
  | VarBound of string * int * int;;

type expr' =
  | Const' of constant
  | Var' of var
  | Box' of var
  | BoxGet' of var
  | BoxSet' of var * expr'
  | If' of expr' * expr' * expr'
  | Seq' of expr' list
  | Set' of expr' * expr'
  | Def' of expr' * expr'
  | Or' of expr' list
  | LambdaSimple' of string list * expr'
  | LambdaOpt' of string list * string * expr'
  | Applic' of expr' * (expr' list)
  | ApplicTP' of expr' * (expr' list);;

let rec expr'_eq e1 e2 =
  match e1, e2 with
  | Const' Void, Const' Void -> true
  | Const'(Sexpr s1), Const'(Sexpr s2) -> sexpr_eq s1 s2
  | Var'(VarFree v1), Var'(VarFree v2) -> String.equal v1 v2
  | Var'(VarParam (v1,mn1)), Var'(VarParam (v2,mn2)) -> String.equal v1 v2 && mn1 = mn2
  | Var'(VarBound (v1,mj1,mn1)), Var'(VarBound (v2,mj2,mn2)) -> String.equal v1 v2 && mj1 = mj2  && mn1 = mn2
  | If'(t1, th1, el1), If'(t2, th2, el2) -> (expr'_eq t1 t2) &&
                                            (expr'_eq th1 th2) &&
                                              (expr'_eq el1 el2)
  | (Seq'(l1), Seq'(l2)
  | Or'(l1), Or'(l2)) -> List.for_all2 expr'_eq l1 l2
  | (Set'(var1, val1), Set'(var2, val2)
  | Def'(var1, val1), Def'(var2, val2)) -> (expr'_eq var1 var2) &&
                                             (expr'_eq val1 val2)
  | LambdaSimple'(vars1, body1), LambdaSimple'(vars2, body2) ->
     (List.for_all2 String.equal vars1 vars2) &&
       (expr'_eq body1 body2)
  | LambdaOpt'(vars1, var1, body1), LambdaOpt'(vars2, var2, body2) ->
     (String.equal var1 var2) &&
       (List.for_all2 String.equal vars1 vars2) &&
         (expr'_eq body1 body2)
  | Applic'(e1, args1), Applic'(e2, args2)
  | ApplicTP'(e1, args1), ApplicTP'(e2, args2) ->
	 (expr'_eq e1 e2) &&
	   (List.for_all2 expr'_eq args1 args2)
  | _ -> false;;
	
                       
exception X_syntax_error;;
exception X_bug_error;;

module type SEMANTICS = sig
  val run_semantics : expr -> expr'
  val annotate_lexical_addresses : expr -> expr'
  val annotate_tail_calls : expr' -> expr'
  val box_set : expr' -> expr'
end;;

module Semantics : SEMANTICS = struct


type env =
  |Nil
  |Env of string list * env;;

let rec get_index v l =
  match l with
  |[] -> raise X_bug_error
  |first::rest ->
    if(first = v)
    then 0
    else
      let acc = (get_index v rest) in
      1+acc;;

let rec handle_bound v env major_index=
  match env with
  |Nil -> Var'(VarFree(v))
  |Env(l,env) ->
    if(List.exists (fun(x)->x=v)) l
    then
      let minor_index = (get_index v l) in
      Var'(VarBound(v,major_index, minor_index))
    else (handle_bound v env (major_index +1))

let handle_var env v =
  let Env(l , prev_env) = env in
  if(List.exists (fun(x)->x=v) l)
  then
    let index = (get_index v l) in
    Var'(VarParam(v, index))
  else (handle_bound v prev_env 0);;

;;
                      
let rec rec_anno_lex env e =
  match e with
  |Const(c) -> Const'(c)
  |Set(e1,e2) -> Set'((rec_anno_lex env e1),(rec_anno_lex env e2))
  |Var(v) -> handle_var env v
  |LambdaSimple(params, body) -> handle_lambda env params body
  |LambdaOpt(params,opt ,body) -> handle_lambda_opt env params opt body
  |Applic(proc, args) -> handle_applic env proc args
  |If(test, dit,dif) -> handle_if env test dit dif
  |Or(args) -> handle_or env args
  |Seq(exps) -> handle_seq env exps
  |Def(var,vall) -> handle_def env var vall
                               
and handle_lambda env params body =
  let env = Env(params, env) in
  let body' = (rec_anno_lex env body) in
  LambdaSimple'(params, body')

and handle_lambda_opt env params opt body =
  let env = Env((List.append params [opt]), env) in
  let body' = (rec_anno_lex env body) in
  LambdaOpt'(params,opt,body')

and handle_applic env proc args =
  let proc' = rec_anno_lex env proc in
  let args' = (map env args) in
  Applic'(proc', args')
  
and handle_if env test dit dif =
  let test'  = rec_anno_lex env test in
  let dit'  = rec_anno_lex env dit in
  let dif'  = rec_anno_lex env dif in
  If'(test',dit',dif')

and handle_or env args =
  Or'((map env args))

and handle_seq env exps =
  Seq'((map env exps))

and map env l=
  let map_func = rec_anno_lex env in
  (List.map map_func l)

and handle_def env var vall =
  let var' = rec_anno_lex env var in
  let vall' = rec_anno_lex env vall in
  Def'(var',vall')
;;


let rec annotate_tc in_tp e =
  match e with
  |Applic'(proc, args) ->
    let proc' = (annotate_tc false proc) in
    let args' = (map false args) in
    if(in_tp)
    then
      ApplicTP'(proc', args')
    else
      Applic'(proc', args')
  |LambdaSimple'(params, body)->
    let body' = annotate_tc true body in
    LambdaSimple'(params, body')
  |LambdaOpt'(params, opt, body)->
    let body' = annotate_tc true body in
    LambdaOpt'(params,opt, body')
  |If'(test, dit, dif)->
    let test'= annotate_tc false test  in
    let dit' = annotate_tc in_tp dit  in
    let dif' = annotate_tc in_tp dif  in
    If'(test',dit', dif')
  | Const'(c) -> Const'(c)
  | Var'(v) -> Var'(v)
  | Seq'(l) ->
     let rec map_list =function
       |[] -> []
       |last::[] -> [(annotate_tc in_tp last)]
       |car::cdr -> (annotate_tc false car)::(map_list cdr)
     in
     let l =(map_list l) in
     Seq'(l)
  | Set'(var,vall) ->
     let var' = annotate_tc false var in
     let vall' = annotate_tc false vall  in
     Set'(var', vall')
  | Def'(var,vall) ->
     let var' = annotate_tc false var  in
     let vall' = annotate_tc false var  in
     Def'(var', vall')
  | Or'(args) ->
     let rec map_list =function
       |[] -> []
       |last::[] -> [(annotate_tc in_tp last)]
       |car::cdr -> (annotate_tc false car)::(map_list cdr)
     in
     let args' =map_list args in
     Or'(args')

and map in_tp l =
  let map_func = annotate_tc in_tp in
  List.map map_func l
;;

let annotate_lexical_addresses e =
  let env = Env([],Nil) in
  rec_anno_lex env e
;;

let annotate_tail_calls e =
  annotate_tc false e 
;;



let box_set e = raise X_not_yet_implemented;;

let run_semantics expr =
  box_set
    (annotate_tail_calls
       (annotate_lexical_addresses expr));;
  
end;; (* struct Semantics *)
