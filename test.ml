open OUnit2;;

let test_char_a test_ctxt = assert_equal (Reader.Char 'a') (Reader.Reader.read_sexpr "#\\a");;
let test_char_esc test_ctxt = assert_equal (Reader.Char '\127') (Reader.Reader.read_sexpr "#\\\127");;
let test_char_out_of_range_128 test_ctxt =
  assert_equal
    0
    (try let res = (Reader.Reader.read_sexpr "#\\\128") in
        1
     with Reader.X_this_should_not_happen -> 0)
let test_char_out_of_range_space test_ctxt =
  assert_equal
    0
    (try let res = (Reader.Reader.read_sexpr "#\\ ") in
        1
     with Reader.X_this_should_not_happen -> 0)
let test_char_newline test_ctxt = assert_equal (Reader.Char (Char.chr 10)) (Reader.Reader.read_sexpr "#\\newline");;



(* Name the test cases and group them together *)
let suite =
"suite">:::
  [
    "test1">:: test_char_a;
    "test2">:: test_char_esc;
    "test3">:: test_char_out_of_range_128;
    "test4">:: test_char_out_of_range_space;
    "test5">:: test_char_newline
  ]
;;

let () =
  run_test_tt_main suite
;;
