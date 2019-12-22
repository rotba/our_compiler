#use "topfind";;
#require "oUnit";;

#use "semantic-analyser.ml";;


open Semantics;;
open Tag_Parser;;
open Reader;;
open OUnit2;;

let rec sexpr_to_string  =
  function
  |Char(x) -> String.make 1  x
  |String(x) -> x
  |Nil-> "Nil"
  |Symbol(x)-> x
  |Number(Float(x))-> string_of_float x
  |Number(Int(x))-> string_of_int x                           
  |Pair(x,y) -> String.concat "" ["Pair( "; (sexpr_to_string x); " , "; (sexpr_to_string y) ;" )"]
  |TaggedSexpr(s,e) -> String.concat " , "["TaggedSexpr("; s;  (sexpr_to_string e) ;")"]
  |TagRef(x)->x
  |_ -> "not_implemented";;


  


let rec exp_to_string  =
  function
  |Const(Sexpr(s)) -> String.concat "" ["Const ( Sexpr ( "; (sexpr_to_string s); " ) )"]
  |Var(v) -> v
  |Set(exp1,exp2) -> String.concat " " ["(set!"; (exp_to_string exp1); (exp_to_string exp2);")"]
  |Seq(list) -> String.concat "" ["Seq([";(seq_to_string list exp_to_string);"])"]
  |Applic(name,params) -> String.concat "" ["Applic( "; (exp_to_string name);" , ["; (seq_to_string params exp_to_string)  ;"] )"]
  |LambdaSimple(params,body) -> String.concat "" ["(LambdaSimple( ";" ( " ; (string_list_to_string params ); " ) " ; ", ["; (exp_to_string body)  ;"] )"]
  |If(e1,e2,e3)-> String.concat "" ["(If( "; (exp_to_string e1 ); " , " ; (exp_to_string e2) ; "," ; (exp_to_string e3)  ;" )"]
  |_->"not_implemented"
    
and seq_to_string params to_string=
  let rec aggregate = function
  |[] -> []
  |f::r -> (to_string f) :: (aggregate r) in
  let params = (aggregate params) in
  String.concat " , " params

and seq'_to_string params to_string=
  let rec aggregate = function
  |[] -> []
  |f::r -> (to_string f) :: (aggregate r) in
  let params = (aggregate params) in
  String.concat " , " params

and string_list_to_string params=
  let rec aggregate = function
  |[] -> []
  |f::r -> f :: (aggregate r) in
  let params = (aggregate params) in
  String.concat " , " params


and exp'_to_string  =
  function
  |Var'(VarFree(v)) -> String.concat "" ["Var' ( VarFree ( "; v; " ) )"]
  |Var'(VarParam(v,i)) -> String.concat "" ["Var' ( VarParam ( "; v;" , ";(string_of_int i) ; " ) )"]
  |Var'(VarBound(v,major, minor)) -> String.concat "" ["Var' ( VarBound ( "; v;" , ";(string_of_int major); " , "; (string_of_int major) ; " ) )"]
  |LambdaSimple'(params, body) -> String.concat "" ["(LambdaSimple'( ";" ( " ; (string_list_to_string params ); " ) " ; ", ["; (exp'_to_string body)  ;"] )"]
  |Applic'(name,params) -> String.concat "" ["Applic'( "; (exp'_to_string name);" , ["; (seq'_to_string params exp'_to_string)  ;"] )"]
  |_->"not_implemented"
  
;;


let assert_equal_expr_tag expr1 expr2=
  assert_equal
    ~cmp: expr'_eq
    ~printer: exp'_to_string
    expr1
    expr2
  ;;


(* Name the test cases and group them together *)
let simple_suite =
"simple suite">:::
  [
    "(lambda(x) (x (lambda (y)(x y (lambda(z) (x y z))))))">::(fun _ ->
      assert_equal_expr_tag
        (LambdaSimple'(
             ["x"],
             Applic'(
                 Var'(VarParam("x",0)),
                 [
                   LambdaSimple'(
                       ["y"],
                       Applic'(
                           Var'(VarBound("x",0,0)),
                           [
                             Var'(VarParam("y",0));
                             LambdaSimple'(
                                 ["z"],
                                 Applic'(
                                     Var'(VarBound("x",1,0)),
                                     [
                                       Var'(VarBound("y",0,0));
                                       Var'(VarParam("z",0))
                                     ]
                                   )
                               )
                           ]
                         )
                     )
                 ]
               )
        ))
        (Semantics.annotate_lexical_addresses(Tag_Parser.tag_parse_expression(
             Reader.read_sexpr(
                 "(lambda(x) (x (lambda (y)(x y (lambda(z) (x y z))))))"
           )))
        )
    );
    "(lambda (x) x)">::(fun _ ->
      assert_equal_expr_tag
        (LambdaSimple'(
             ["x"],
             Var'(VarParam("x", 0))
        ))
        (Semantics.annotate_lexical_addresses(Tag_Parser.tag_parse_expression(
             Reader.read_sexpr(
                 "(lambda(x) x)"
           )))
        )
    )
  ];;


let last_year =
"last year">:::
  [
    "_1">::(fun _ ->
      assert_equal_expr_tag
        (
          Applic' (LambdaSimple' (["x"], If' (Applic' (Var' (VarParam ("x", 0)), [Const' (Sexpr (Number (Int (1))))]), Applic' (Var' (VarParam ("x", 0)), [Const' (Sexpr (Number (Int (2))))]), Applic' (LambdaSimple' (["x"], Set' (Var' (VarParam ("x", 0)), Const' (Sexpr (Number (Int (0)))))), [Const' (Sexpr (Number (Int (3))))]))), [LambdaSimple' (["x"], Var' (VarParam ("x", 0)))])
        )
        (Semantics.annotate_lexical_addresses(
             Applic
               (LambdaSimple (["x"],
                              If (Applic (Var "x", [Const (Sexpr (Number (Int 1)))]),
                                  Applic (Var "x", [Const (Sexpr (Number (Int 2)))]),
                                  Applic
                                    (LambdaSimple (["x"], Set (Var "x", Const (Sexpr (Number (Int 0))))),
                                     [Const (Sexpr (Number (Int 3)))]))),
                [LambdaSimple (["x"], Var "x")])
           )
        )
    );
  ];;

let () =
  run_test_tt_main simple_suite;
  run_test_tt_main last_year;
;;
