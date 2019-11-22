open OUnit2;;
let rec sexpr_to_string  =
  function
  |Reader.Char(x) -> String.make 1  x
  |Reader.String(x) -> x
  |Reader.Nil-> "Nil"
  |Reader.Symbol(x)-> x
  |Reader.Pair(x,y) -> String.concat "" ["Pair( "; (sexpr_to_string x); " , "; (sexpr_to_string y) ;" )"]
  |_ -> "not_implemented";;

let assert_equal_sexpr sexpr1 sexpr2=
  assert_equal
    ~cmp: Reader.sexpr_eq
    ~printer:sexpr_to_string
    sexpr1
    sexpr2
  ;;




(* Name the test cases and group them together *)
let char_suite =
"char suite">:::
  [
    "a">::(fun _ -> assert_equal_sexpr (Reader.Char 'a') (Reader.Reader.read_sexpr "#\\a"));
    "esc">:: (fun _ ->assert_equal_sexpr (Reader.Char '\127') (Reader.Reader.read_sexpr "#\\\127"));
    "out_of_range_128">:: (fun _ ->assert_raises
    Reader.PC.X_no_match
    (fun _ ->
      let res = (Reader.Reader.read_sexpr "#\\\128") in
      ()));
    "out_of_range_space">:: (fun _ -> assert_raises
    Reader.PC.X_no_match
    (fun _ ->
      let res = (Reader.Reader.read_sexpr "#\\\ ") in
      ()));
    "newline">:: (fun _ ->assert_equal_sexpr (Reader.Char (Char.chr 10)) (Reader.Reader.read_sexpr "#\\newline"));
    "page in sensitive">:: (fun _ ->assert_equal_sexpr (Reader.Char (Char.chr 12)) (Reader.Reader.read_sexpr "#\\Page"));
    "with whitespaces">:: (fun _ ->assert_equal_sexpr (Reader.Char 'a') (Reader.Reader.read_sexpr "   #\\a    "));
    "with comment">:: (fun _ ->assert_equal_sexpr (Reader.Char (Char.chr 12)) (Reader.Reader.read_sexpr ";this is a comment\n#\\Page"));
    "with comments">:: (fun _ ->  assert_equal_sexpr (Reader.Char (Char.chr 12)) (Reader.Reader.read_sexpr ";this is a comment\n;this is a comment\n#\\Page"));
    "with sexpr comment">:: (fun _ -> assert_equal_sexpr (Reader.Char 'a') (Reader.Reader.read_sexpr "#;\"moshe\"#\\a"))
  ];;

let string_suite =
"string_suite">:::
  [
    "moshe">:: (fun _ -> assert_equal_sexpr (Reader.String "moshe") (Reader.Reader.read_sexpr "\"moshe\""));
    "long string">:: (fun _ -> assert_equal_sexpr
    (Reader.String
       "This is a very longstring that don't spills acrossseveral lines."
    )
    (Reader.Reader.read_sexpr
       "\"This is a very longstring that don't spills acrossseveral lines.\""
    ));
    "with meta">:: (fun _ ->assert_equal_sexpr
    (Reader.String
       "This is a very long\nstring that spills across\nseveral lines."
    )
    (Reader.Reader.read_sexpr
       "\"This is a very long\nstring that spills across\nseveral lines.\""
    ));
    "empty str">:: (fun _ -> assert_equal_sexpr (Reader.String "") (Reader.Reader.read_sexpr "\"\""));
    "with meta ci">:: (fun _ -> assert_equal_sexpr
    (Reader.String
       "This is a very long\nstring that spills across\nseveral lines."
    )
    (Reader.Reader.read_sexpr
       "\"This is a very long\Nstring that spills across\nseveral lines.\""
    ));
    "with whitespaces">:: (fun _ -> assert_equal_sexpr (Reader.String "moshe") (Reader.Reader.read_sexpr "   \"moshe\"    "));
    "with comment">:: (fun _ -> assert_equal_sexpr (Reader.String "moshe") (Reader.Reader.read_sexpr ";this is a comment\n\"moshe\""));
    "with comments">:: (fun _ -> assert_equal_sexpr (Reader.String "moshe") (Reader.Reader.read_sexpr ";this is a comment\n\"moshe\";this is also a comment"));
    "with nested sexpr comments">:: (fun _ -> assert_equal_sexpr (Reader.String "moshe") (Reader.Reader.read_sexpr "#;#t\  #;#\\a \"moshe\""))
  ];;

let list_suite =
"list_suite">:::
  [
    "(a b c)">::
      (fun _ ->
        assert_equal_sexpr
          (Reader.Pair(Reader.Char 'a', Pair(Reader.Char 'b', Pair(Reader.Char 'c',Nil ))) )
          (Reader.Reader.read_sexpr "(#\\a #\\b #\\c)")
      );
    "()">::
      (fun _ ->
        assert_equal_sexpr
          (Reader.Nil )
          (Reader.Reader.read_sexpr "()")
      );
    "(a (b c))">::
      (fun _ ->
        assert_equal_sexpr
          (Reader.Pair(
               Reader.Char 'a',
               Pair(
                   Pair(Reader.Char 'b', Pair(Reader.Char 'c',Reader.Nil )),
                   Nil)
             )
          )
          (Reader.Reader.read_sexpr "(#\\a (#\\b #\\c))")
      );
    "(a         b #;#t c)">::
      (fun _ ->
        assert_equal_sexpr
          (Reader.Pair(Reader.Char 'a', Pair(Reader.Char 'b', Pair(Reader.Char 'c',Reader.Nil))) )
          (Reader.Reader.read_sexpr "(          #\\a     #\\b  #;#t #\\c  )")
      );
  ];;

let qoute_suite =
"qoute suite">:::
  [
    "'a">::
      (fun _ ->
        assert_equal_sexpr
          (Pair(Symbol("qoute"),Pair(Char 'a', Nil)))
          (Reader.Reader.read_sexpr "'#\\a")
      );
    "'(a b c)">::
      (fun _ ->
        assert_equal_sexpr
          (Pair(
               Symbol("qoute"),
               Pair(
                   Reader.Pair(Reader.Char 'a', Pair(Reader.Char 'b', Pair(Reader.Char 'c',Nil ))),
                   Nil)
          ))
          (Reader.Reader.read_sexpr "'(#\\a #\\b #\\c)")
      )
  ];;





let () =
  run_test_tt_main string_suite;
  run_test_tt_main char_suite;
  run_test_tt_main list_suite;
  run_test_tt_main qoute_suite
;;
