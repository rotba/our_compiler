#use "topfind";;
#require "oUnit";;

#use "tag-parser.ml";;

open Tag_Parser;;
open Reader;;
open OUnit2;;
let rec expr_to_string  =
  function
  |_ -> "not_implemented";;

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


let () =
  run_test_tt_main simple_suite;
;;
