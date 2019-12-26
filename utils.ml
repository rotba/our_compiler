#use "topfind";;
#use "semantic-analyser.ml"


open Semantics;;
open Tag_Parser;;
open Reader;;

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
  |If'(e1,e2,e3)-> String.concat "" ["(If'( "; (exp'_to_string e1 ); " , " ; (exp'_to_string e2) ; "," ; (exp'_to_string e3)  ;" )"]
  |_->"not_implemented"
  
;;


