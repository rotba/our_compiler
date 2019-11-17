open OUnit2;;

module Tok_char_test: sig
  val a : 'a -> unit
  val esc :  'a -> unit
  val out_of_range_128 : 'a -> unit
  val newline : 'a -> unit
  val out_of_range_space : 'a -> unit
  val page_is : 'a-> unit
end
= struct
let a test_ctxt = assert_equal (Reader.Char 'a') (Reader.Reader.read_sexpr "#\\a");;
let esc test_ctxt = assert_equal (Reader.Char '\127') (Reader.Reader.read_sexpr "#\\\127");;
let out_of_range_128 test_ctxt =
  assert_raises
    Reader.PC.X_no_match
    (fun _ ->
      let res = (Reader.Reader.read_sexpr "#\\\128") in
      ())
  
let out_of_range_space test_ctxt =
  assert_raises
    Reader.PC.X_no_match
    (fun _ ->
      let res = (Reader.Reader.read_sexpr "#\\\ ") in
      ())
let newline test_ctxt = assert_equal (Reader.Char (Char.chr 10)) (Reader.Reader.read_sexpr "#\\newline");;
let page_is test_ctxt = assert_equal (Reader.Char (Char.chr 12)) (Reader.Reader.read_sexpr "#\\Page");;  
end;; (* struct Tok_char_test *)




(* Name the test cases and group them together *)
let suite_char =
"suite char">:::
  [
    "a">:: Tok_char_test.a;
    "esc">:: Tok_char_test.esc;
    "out_of_range_128">:: Tok_char_test.out_of_range_128;
    "out_of_range_space">:: Tok_char_test.out_of_range_space;
    "newline">:: Tok_char_test.newline;
    "page in sensitive">:: Tok_char_test.page_is
  ];;

let () =
  run_test_tt_main suite_char
;;
