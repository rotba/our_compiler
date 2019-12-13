#use "topfind";;
#require "oUnit";;

#use "tag-parser.ml";;


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
  |TaggedSexpr(s,e) -> String.concat "" ["TaggedSexpr( "; s; " , "; (sexpr_to_string e) ;" )"]
  |TagRef(x)->x
  |_ -> "not_implemented";;

let rec expr_to_string  =
  function
  |Const(Sexpr(s)) -> String.concat "" ["Const ( Sexpr ( "; (sexpr_to_string s); " ) )"]
  |_->"not_implemented";;

let assert_equal_expr expr1 expr2=
  assert_equal
    ~cmp: expr_eq
    ~printer:expr_to_string
    expr1
    expr2
  ;;


(* Name the test cases and group them together *)
let simple_suite =
"simple suite">:::
  [
    "5">::(fun _ ->
      assert_equal_expr
        (Const (Sexpr (Number (Int 5))))
        (tag_parse_expression (Number  (Int 5))));
    "moshe">::(fun _ ->
      assert_equal_expr
        (Const (Sexpr (String "moshe")))
        (tag_parse_expression (String  "moshe")));
  ];;

let less_simple_suite =
    "less simple suite">:::
      [
        "tagged def special case">::(fun _ ->
          assert_equal_expr
          (Const(Sexpr (TaggedSexpr ("x", Nil))))
          (tag_parse_expression (TaggedSexpr ("x", Pair (Symbol "quote", Pair (Nil, Nil)))))
        );
        "tag ref">::(fun _ ->
          assert_equal_expr
          (Const(Sexpr (TagRef "x")))
          (tag_parse_expression (TagRef "x"))
        );
        "1.(a)">::(fun _ ->
          assert_equal_expr
            (Const(Sexpr (TaggedSexpr ("x", Pair(Symbol "quote",Pair (Nil, Nil))))))
            (tag_parse_expression (Pair(Symbol "quote", Pair(TaggedSexpr("x", Pair (Symbol "quote", Pair (Nil, Nil))), Nil))))
        );
      ];;


let () =
  run_test_tt_main simple_suite;
  run_test_tt_main less_simple_suite;
;;
