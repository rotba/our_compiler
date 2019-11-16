open OUnit2;;

let test_char_a test_ctxt = assert_equal (Reader.Char 'a') (Reader.Reader.read_sexpr "#\\a");;
let test_char_esc test_ctxt = assert_equal (Reader.Char '\127') (Reader.Reader.read_sexpr "#\\\127");;
let test_char_out_of_range test_ctxt =
  assert_equal
    0
    (try let res = (Reader.Reader.read_sexpr "#\\\128") in
        1
    with Reader.X_this_should_not_happen -> 0)



(* Name the test cases and group them together *)
let suite =
"suite">:::
  [
    "test1">:: test_char_a;
    "test2">:: test_char_esc;
    "test3">:: test_char_out_of_range
  ]
;;

let () =
  run_test_tt_main suite
;;
