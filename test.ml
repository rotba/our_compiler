open OUnit2;;
let sexpr_to_string  sexpr=
  match sexpr with
  |Reader.Char(x) -> String.make 1  x
  |Reader.String(x) ->  x
  |_ -> "not_implemented";;

let assert_equal_sexpr sexpr1 sexpr2=
  assert_equal
    ~cmp: Reader.sexpr_eq
    ~printer:sexpr_to_string
    sexpr1
    sexpr2
  ;;
module Tok_char_test: sig
  val a : 'a -> unit
  val esc :  'a -> unit
  val out_of_range_128 : 'a -> unit
  val newline : 'a -> unit
  val out_of_range_space : 'a -> unit
  val page_is : 'a-> unit
end
= struct
let a test_ctxt = assert_equal_sexpr (Reader.Char 'a') (Reader.Reader.read_sexpr "#\\a");;
let esc test_ctxt = assert_equal_sexpr (Reader.Char '\127') (Reader.Reader.read_sexpr "#\\\127");;
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
  
let newline test_ctxt = assert_equal_sexpr (Reader.Char (Char.chr 10)) (Reader.Reader.read_sexpr "#\\newline");;
let page_is test_ctxt = assert_equal_sexpr (Reader.Char (Char.chr 12)) (Reader.Reader.read_sexpr "#\\Page");;  
end;; (* struct Tok_char_test *)

module Tok_string_test: sig
  val moshe : 'a -> unit
  val long_str :'a -> unit
  val with_meta : 'a -> unit
end
= struct
let moshe test_ctxt = assert_equal_sexpr (Reader.String "moshe") (Reader.Reader.read_sexpr "\"moshe\"");;
let long_str test_ctxt =
  assert_equal_sexpr
    (Reader.String
       "This is a very longstring that don't spills acrossseveral lines."
    )
    (Reader.Reader.read_sexpr
       "\"This is a very longstring that don't spills acrossseveral lines.\""
    );;
let with_meta test_ctxt =
  assert_equal_sexpr
    (Reader.String
       "This is a very long\nstring that spills across\nseveral lines."
    )
    (Reader.Reader.read_sexpr
       "\"This is a very long\nstring that spills across\nseveral lines.\""
    );;
  
  
end;; (* struct Tok_string_test *)




(* Name the test cases and group them together *)
let char_suite =
"char suite">:::
  [
    "a">:: Tok_char_test.a;
    "esc">:: Tok_char_test.esc;
    "out_of_range_128">:: Tok_char_test.out_of_range_128;
    "out_of_range_space">:: Tok_char_test.out_of_range_space;
    "newline">:: Tok_char_test.newline;
    "page in sensitive">:: Tok_char_test.page_is
  ];;

let string_suite =
"string_suite">:::
  [
    "moshe">:: Tok_string_test.moshe;
    "long string">:: Tok_string_test.long_str;
    "with meta">:: Tok_string_test.with_meta
  ];;

let () =
  run_test_tt_main char_suite;
  run_test_tt_main string_suite
;;
