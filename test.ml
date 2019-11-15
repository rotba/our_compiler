open OUnit2;;

let test1 test_ctxt = assert_equal (Reader.Char 'a') (Reader.Reader.read_sexpr "#\\a");;



(* Name the test cases and group them together *)
let suite =
"suite">:::
 ["test1">:: test1]
;;

let () =
  run_test_tt_main suite
;;
