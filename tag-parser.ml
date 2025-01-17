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

let rec exp_to_string  =
  function
  |Const Void -> "Void"
  |Const(Sexpr(s)) -> String.concat "" ["Const ( Sexpr ( "; (sexpr_to_string s); " ) )"]
  |Var(v) -> v
  |Set(exp1,exp2) -> String.concat " " ["(set!"; (exp_to_string exp1); (exp_to_string exp2);")"]
  |Seq(list) -> String.concat "" ["Seq([";(seq_to_string list exp_to_string);"])"]
  |Applic(name,params) -> String.concat "" ["Applic( "; (exp_to_string name);" , ["; (seq_to_string params exp_to_string)  ;"] )"]
  |LambdaSimple(params,body) -> String.concat "" ["(LambdaSimple( ";" ( " ; (string_list_to_string params ); " ) " ; ", ["; (exp_to_string body)  ;"] )"]
  |If(e1,e2,e3)-> String.concat "" ["(If( "; (exp_to_string e1 ); " , " ; (exp_to_string e2) ; "," ; (exp_to_string e3)  ;" )"]
  |_ -> "not_implemented to_string"
;;

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
	
                       
exception X_syntax_error of string;;

let handle_tagged first sec =
  let rec sec_handler = function
    | Pair (Symbol ("quote"), Pair (a, Nil)) -> a
    | Pair(a,b) -> Pair((sec_handler a),(sec_handler b))
    | x-> x in
  let sec = sec_handler sec in
  Const(Sexpr(TaggedSexpr(first,sec)));;

let rec is_proper = function
  | Nil -> true
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
  | Pair(Symbol("quasiquote"),Pair(cdr,Nil)) -> handle_qq(cdr)
  | Pair(Symbol("let"),cdr) -> handle_let(cdr)
  | Pair(Symbol("let*"),cdr) -> handle_let_star(cdr)
  | Pair(Symbol("letrec"),cdr) -> handle_letrec(cdr)
  | Number(x) -> (Const (Sexpr (Number x)))
  | String(x) -> (Const (Sexpr(String x)))
  | Bool(x) -> (Const (Sexpr(Bool x)))
  | Char(x) -> (Const (Sexpr(Char x)))
  | TagRef(x) -> (Const (Sexpr(TagRef x)))
  | TaggedSexpr(first, sec) ->(handle_tagged first sec)
  | Pair(Symbol("quote"), Pair(sec,Nil)) ->Const(Sexpr(sec))
  | Symbol(s) ->if (List.exists (fun(e)-> e=s) reserved_word_list) then raise (X_syntax_error "tag_parse_expression") else (Var(s))
  | Pair(Symbol "if", Pair(a, Pair(b, Pair(c, _)))) -> If ((tag_parse_expression (a)), (tag_parse_expression (b)), (tag_parse_expression (c)))
  | Pair(Symbol "if", Pair(a, Pair(b, _))) -> If ((tag_parse_expression (a)), (tag_parse_expression (b)), Const(Void))
  | Pair(Symbol("lambda"), cdr) ->(handle_lambda cdr)
  | Pair(Symbol("or"), cdr) ->(handle_or cdr)
  | Pair(Symbol("set!"), Pair(name, Pair(value,Nil))) ->Set(tag_parse_expression(name),tag_parse_expression(value) )
  | Pair(Symbol "define", Pair(Symbol(a), Pair(b, Nil))) -> Def ((tag_parse_expression (Symbol(a))), (tag_parse_expression (b)))
  | Pair(Symbol "define", a) -> handle_define a
  | Pair(Symbol "begin", a) -> handle_begin a
  | Pair(Symbol "cond", a) -> handle_cond a
  | Pair(Symbol "and", a) -> handle_and a
  (*################################################################################# *)
  | Pair(a, b) -> Applic ((tag_parse_expression a), (parse_applic_body b))              
  |_ -> raise (X_syntax_error "tag_parse_expression")

  and handle_define sexpr = 
  let rec build_and = function
    | Pair(Pair(Symbol(a),b), c) -> 
    Pair (Symbol "define", 
        Pair( Symbol (a),
          Pair(
            Pair(Symbol "lambda", 
                Pair(b,
                  c)) ,
        Nil)))
    | _ -> raise (X_syntax_error "handle_define") in 

    tag_parse_expression (build_and sexpr)


  and handle_and sexpr = 
let rec build_and = function 
  | Nil -> Bool(true)
  | Pair(a, Nil) -> a
  | Pair(a, b) -> (Pair (Symbol "if",Pair (a, Pair (Pair(Symbol "and", b) , Pair (Bool(false), Nil))))) 
  | _ -> raise Exhausting in
  
  tag_parse_expression (build_and sexpr)



and handle_cond sexpr = 
let rec build_cond = function
  | Nil -> Nil
  | Pair(Pair(a, Pair(Symbol("=>"), b)), Nil) ->Pair(
  Pair (Symbol "let",
    Pair
     (Pair
       (Pair (Symbol "value",
         Pair (a, Nil)),
       Pair
        (Pair (Symbol "f",
          Pair
           (Pair (Symbol "lambda",
             Pair (Nil,
              b)),
           Nil)),
        Nil)),
     Pair
      (Pair (Symbol "if",
        Pair (Symbol "value",
         Pair (Pair (Pair (Symbol "f", Nil), Pair (Symbol "value", Nil)),
           Nil))),
      Nil))),
  Nil)
  | Pair(Pair(a, Pair(Symbol("=>"), b)), c) ->Pair(
  Pair (Symbol "let",
    Pair
     (Pair
       (Pair (Symbol "value",
         Pair (a, Nil)),
       Pair
        (Pair (Symbol "f",
          Pair
           (Pair (Symbol "lambda",
             Pair (Nil,
              b)),
           Nil)),
        Pair
         (Pair (Symbol "rest",
           Pair
            (Pair (Symbol "lambda",
              Pair (Nil,
               (build_cond c))),
            Nil)),
         Nil))),
     Pair
      (Pair (Symbol "if",
        Pair (Symbol "value",
         Pair (Pair (Pair (Symbol "f", Nil), Pair (Symbol "value", Nil)),
          Pair (Pair (Symbol "rest", Nil), Nil)))),
      Nil))),
  Nil)
  (* Pair(Symbol "let", Pair( Pair( Pair(Symbol "value", a), Pair(Pair(Symbol "f", Pair( Symbol "lambda")))))) *)
  | Pair(Pair(Symbol("else"), b), _  ) ->  Pair(Pair(Symbol("begin"), b),Nil)
  | Pair(Pair(a, b), c) ->  Pair(Pair(Symbol("if"), Pair(a, Pair(Pair(Symbol("begin"),b), (build_cond c)))), Nil)
  | _ -> raise Exhausting in
let extract = function
  | Pair(a, b) -> a 
  | _ -> raise Exhausting in
  (tag_parse_expression (extract (build_cond sexpr)))


  
and parse_applic_body = function
  | Nil -> []
  | Pair(a, b) -> tag_parse_expression(a)::parse_applic_body(b)
  | _ -> raise Exhausting  

and handle_begin = function
  |Nil -> Const(Void)
  |Pair(car, Nil) -> (tag_parse_expression  car)
  |Pair(car, cdr) ->(Seq(parse_applic_body (Pair(car, cdr))))
  | _->raise (X_syntax_error "parse_applic_body")


  
and handle_lambda cdr =
  let rec to_list = function
  |Nil-> []
  |Pair(Symbol(v),cdr)-> (List.append [v] (to_list cdr))
  | _ -> raise Exhausting in

  let rec handle_body = function
    |Pair(car, Nil) -> (tag_parse_expression  car)
    |Pair(car, cdr) ->(Seq(parse_applic_body (Pair(car, cdr))))
    | _->raise (X_syntax_error "handle_body")  in

  let rec get_opt_list = function
    |Pair(Symbol(vn),Symbol(vs))->([vn], vs)
    |Pair(Symbol(vn_m_1), cdr) ->
      let (acc, vs) = (get_opt_list cdr) in
      (vn_m_1::acc,vs)
    |Symbol(vs) -> ([],vs)
    |_ -> raise (X_syntax_error "get_opt_list") in
       
  match cdr with
  |Pair(params, body)->
    if((is_proper params))
    then LambdaSimple((to_list params), (handle_body body))
    else
      let (params, opt) = (get_opt_list params) in
      LambdaOpt(params, opt, (handle_body body))
  |_ -> raise (X_syntax_error "get_opt_list")
 
and handle_or cdr =

let rec build_or = function
| Nil -> []
| Pair(car, cdr) -> tag_parse_expression(car)::build_or(cdr)  
| _ -> raise Exhausting in

match cdr with
| Nil -> tag_parse_expression (Bool(false))
| Pair(car, Nil) -> tag_parse_expression (car)
| Pair(car, cdr) -> Or(build_or((Pair(car, cdr))) )
| _ -> raise (X_syntax_error "build_or")
     
and expand_qq =
  let quote_wrap s = Pair(Symbol("quote"), Pair(Symbol(s),Nil)) in
  let quote_wrap_nil = Pair(Symbol("quote"), Pair(Nil,Nil)) in
  let cons_wrap a b = Pair(Symbol("cons"), Pair(a,Pair(b, Nil))) in
  let append_wrap a b = Pair(Symbol("append"), Pair(a,Pair(b, Nil))) in
  function
  |Pair(Symbol("unquote"), Pair(cdr,Nil)) -> cdr
  |Pair(Symbol("unquote-splicing"), Pair(cdr,Nil)) -> raise (X_syntax_error "expand_qq 1")
  |Symbol(s) -> quote_wrap(s)
  |Nil -> quote_wrap_nil
  |Pair(Pair(Symbol("unquote-splicing"), Pair(a,Nil)),b) ->
    let b = expand_qq(b) in
    (append_wrap a b)
  |Pair(a, Pair(Symbol("unquote-splicing"), Pair(b,Nil))) ->
    let a = expand_qq(a) in
    (cons_wrap a b)
  |Pair(a,b) ->
    let a = expand_qq(a) in
    let b = expand_qq(b) in
    (cons_wrap a b)
  | a -> a 
  (* raise (X_syntax_error (Printf.sprintf "expand_qq 2: %s" (sexpr_to_string a))) *)

and expand_let =
  let rec get_bindings =
    let get_binding = function
      |Pair(var,Pair(vaal, Nil)) -> (var, vaal)
      | _ -> raise (X_syntax_error "expand_let 1") in
    function
    |Nil -> (Nil,Nil)
    |Pair(car,cdr) ->(
      let (var, vaal) = (get_binding car) in
      let (vars, vals) = (get_bindings cdr) in
      (Pair(var,vars), Pair(vaal,vals)))
    | _ -> raise (X_syntax_error "expand_let 2") in
  let lambda_wrap vars body =
    Pair(
        Symbol("lambda"),
        Pair(vars, body)
      ) in
  let applic_wrap lambda vals =
    Pair(lambda,vals) in
  function
  |Pair(ribs, cdr) ->
    let (vars, vals) = (get_bindings ribs) in
    let lambda = (lambda_wrap vars cdr) in
    let applic = (applic_wrap lambda vals) in
    applic
  |_ -> raise (X_syntax_error "expand_let 3")

and expand_let_star =
  let let_star_wrap ribs body =
    Pair(Symbol("let*"),Pair(ribs, body)) in
  let let_wrap rib1 rest =
    Pair(
        Symbol("let"),
        Pair(
            Pair(rib1, Nil),
            Pair(rest,Nil)
          )
      ) in
  function
  |Pair(ribs, body) ->(
    match ribs with
    |Nil -> (expand_let (Pair(ribs,body)))
    |Pair(_,Nil) -> (expand_let (Pair(ribs,body)))
    |Pair(rib_1, cdr) ->
      let rest = (let_star_wrap cdr body) in
      let_wrap rib_1 rest
    |_ -> raise (X_syntax_error "expand_let_star 1"))
  | _ -> raise (X_syntax_error "expand_let_star 2")
and expand_letrec =
  let rec whatevers_wrap =
    let gen_whatever var = Pair(var, Pair(Pair(Symbol("quote"), Pair(Symbol("whatever"),Nil)), Nil)) in
    function
    |Nil -> Nil
    |Pair(Pair(var, _),cdr) ->
      let whatever = gen_whatever var in
      let rest = whatevers_wrap cdr in
      Pair(whatever, rest)
    |_ -> raise (X_syntax_error "expand_letrec")
  in
  (* let empty_let_wrap body=Pair(Symbol("let"),Pair(Nil,body)) *)
  let gen_body ribs body =
    let rec set_bangs_wrap =function
    |Nil -> body
    |Pair(car,cdr) ->
      let set_bang = Pair(Symbol("set!"),car) in
      let rest = set_bangs_wrap cdr in
      Pair(set_bang, rest)
    |_ -> raise (X_syntax_error "set_bangs_wrap") in
    set_bangs_wrap ribs
  in
  let let_wrap ribs body =
    Pair(
        Symbol("let"),
        Pair(ribs, body)
      )
  in
  function
  |Pair(ribs, body) ->
    let whatevers = whatevers_wrap ribs in
    let body = gen_body ribs body in
    let_wrap whatevers body
  |_ -> raise (X_syntax_error "gen_body")

and handle_qq sexp=
  let expanded = expand_qq sexp in
  tag_parse_expression expanded

and handle_let sexp =
  let expanded = expand_let sexp in
  tag_parse_expression expanded
  
and handle_let_star sexp =
  let expanded = expand_let_star sexp in
  tag_parse_expression expanded

and handle_letrec sexp =
  let expanded = expand_letrec sexp in
  tag_parse_expression expanded
;;



let tag_parse_expressions sexpr = 
  List.map tag_parse_expression sexpr;;

  end;; (* struct Tag_Parser *) 
