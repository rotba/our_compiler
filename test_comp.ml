#use "topfind";;
#require "oUnit";;

#use "compiler.ml";;


open Semantics;;
open Tag_Parser;;
open Reader;;
open OUnit2;;



let assert_equal_expr_tag expr1 expr2=
  assert_equal
    (* ~cmp: expr'_eq *)
    ~printer: exp'_to_string
    expr1
    expr2
  ;;


(* Name the test cases and group them together *)
let comp =
"comp">:::
  [
    "comp">::(fun _ ->
      assert_equal
        1
        2
    )
  ];;



  

let () =
  run_test_tt_main comp;
  (* run_test_tt_main single; *)
;;
