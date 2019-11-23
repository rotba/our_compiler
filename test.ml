open OUnit2;;
let rec sexpr_to_string  =
  function
  |Reader.Char(x) -> String.make 1  x
  |Reader.String(x) -> x
  |Reader.Nil-> "Nil"
  |Reader.Symbol(x)-> x
  |Reader.Number(Float(x))-> string_of_float x
  |Reader.Number(Int(x))-> string_of_int x                           
  |Reader.Pair(x,y) -> String.concat "" ["Pair( "; (sexpr_to_string x); " , "; (sexpr_to_string y) ;" )"]
  |Reader.TaggedSexpr(s,e) -> String.concat "" ["TaggedSexpr( "; s; " , "; (sexpr_to_string e) ;" )"]
  |Reader.TagRef(x)->x
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
    (* "(#;#T   ;asjdfasdkjsadjhka\n)">::
     *   (fun _ ->
     *     assert_equal_sexpr
     *       (Reader.Nil )
     *       (Reader.Reader.read_sexpr "(#;#T   ;asjdfasdkjsadjhka\n)")
     *   ); *)
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

     "(a         b #;#t .c)">::
      (fun _ ->
        assert_equal_sexpr
          (Reader.Pair(Reader.Char 'a', Pair(Reader.Char 'b', Reader.Char 'c')) )
          (Reader.Reader.read_sexpr "(#\\a #\\b .#\\c)")
      );
     "(a b . c)">::
      (fun _ ->
        assert_equal_sexpr
          (Reader.Pair(Reader.Char 'a', Pair(Reader.Char 'b', Reader.Char 'c')) )          
          (Reader.Reader.read_sexpr "(          #\\a     #\\b  #;#t .#\\c  )")
      );
    "(a .(b c))">::
      (fun _ ->
        assert_equal_sexpr
          (Reader.Pair(
               Reader.Char 'a',
               Pair(Reader.Char 'b', Pair(Reader.Char 'c',Reader.Nil ))
          ))
          (Reader.Reader.read_sexpr "(#\\a .(#\\b #\\c))")
      );     
  ];;

let qouted_forms_suite =
"qouted forms suite">:::
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
      );
        ",a">::
      (fun _ ->
        assert_equal_sexpr
          (Pair(Symbol("unquote"),Pair(Char 'a', Nil)))
          (Reader.Reader.read_sexpr ",#\\a")
      );
    ",(a b c)">::
      (fun _ ->
        assert_equal_sexpr
          (Pair(
               Symbol("unquote"),
               Pair(
                   Reader.Pair(Reader.Char 'a', Pair(Reader.Char 'b', Pair(Reader.Char 'c',Nil ))),
                   Nil)
          ))
          (Reader.Reader.read_sexpr ",(#\\a #\\b #\\c)")
      );
    ",@a">::
      (fun _ ->
        assert_equal_sexpr
          (Pair(Symbol("unquote-splicing"),Pair(Char 'a', Nil)))
          (Reader.Reader.read_sexpr ",@#\\a")
      );
     ",@#t">::
      (fun _ ->
        assert_equal_sexpr
          (Pair(Symbol("unquote-splicing"),Pair(Bool true, Nil)))
          (Reader.Reader.read_sexpr ",@#t")
      );
  ];;


let number_suite =
"number suite">:::
  [
    "1">::
      (fun _ ->
        assert_equal_sexpr
          (Number (Int 1))
          (Reader.Reader.read_sexpr "1")
      );
    "+1">::
      (fun _ ->
        assert_equal_sexpr
          (Number (Int 1))
          (Reader.Reader.read_sexpr "+1")
      );
    "-1">::
      (fun _ ->
        assert_equal_sexpr
          (Number (Int (-1)))
          (Reader.Reader.read_sexpr "-1")
      );
    "1234">::
      (fun _ ->
        assert_equal_sexpr
          (Number (Int 1234))
          (Reader.Reader.read_sexpr "1234")
      );
    "01234">::
      (fun _ ->
        assert_equal_sexpr
          (Number (Int 1234))
          (Reader.Reader.read_sexpr "01234")
      );
    "-01234">::
      (fun _ ->
        assert_equal_sexpr
          (Number (Int (-1234)))
          (Reader.Reader.read_sexpr "-01234")
      );
    "+01234">::
      (fun _ ->
        assert_equal_sexpr
          (Number (Int 1234))
          (Reader.Reader.read_sexpr "+01234")
      );
    "-0">::
      (fun _ ->
        assert_equal_sexpr
          (Number (Int 0))
          (Reader.Reader.read_sexpr "-0")
      );
    "0005.0129">::
      (fun _ ->
        assert_equal_sexpr
          (Number (Float 5.0129))
          (Reader.Reader.read_sexpr "0005.0129")
      );
    "501.100000000000000000000">::
      (fun _ ->
        assert_equal_sexpr
          (Number (Float 501.1))
          (Reader.Reader.read_sexpr "501.100000000000000000000")
      );
    "-0.0">::
      (fun _ ->
        assert_equal_sexpr
          (Number (Float 0.0))
          (Reader.Reader.read_sexpr "-0.0")
      );
    "+999.12349999999">::
      (fun _ ->
        assert_equal_sexpr
          (Number (Float 999.12349999999))
          (Reader.Reader.read_sexpr "+999.12349999999")
      );
    "1e1">::
      (fun _ ->
        assert_equal_sexpr
          (Number (Float 10.0))
          (Reader.Reader.read_sexpr "1e1")
      );
    "1E+1">::
      (fun _ ->
        assert_equal_sexpr
          (Number (Float 10.0))
          (Reader.Reader.read_sexpr "1E+1")
      );
    "10e-1">::
      (fun _ ->
        assert_equal_sexpr
          (Number (Float 1.0))
          (Reader.Reader.read_sexpr "10e-1")
      );
    "3.14e+9">::
      (fun _ ->
        assert_equal_sexpr
          (Number (Float 3.14e+9))
          (Reader.Reader.read_sexpr "3.14e+9")
      );
    "3.14E-512">::
      (fun _ ->
        assert_equal_sexpr
          (Number (Float 3.14E-512))
          (Reader.Reader.read_sexpr "3.14E-512")
      );
    "+000000012.3E00000002">::
      (fun _ ->
        assert_equal_sexpr
          (Number (Float 1230.0))
          (Reader.Reader.read_sexpr "+000000012.3E00000002")
      );
    "-5.000000000e-2">::
      (fun _ ->
        assert_equal_sexpr
          (Number (Float (-0.05)))
          (Reader.Reader.read_sexpr "-5.000000000e-2")
      );
    "#-1r1">::
      (fun _ ->
        assert_raises
          Reader.PC.X_no_match
          (fun _->
            let _i = (Reader.Reader.read_sexpr "#-1r1") in
            ()
          )
      );
    "#36rZZ">::
      (fun _ ->
        assert_equal_sexpr
          (Number (Int 1295))
          (Reader.Reader.read_sexpr "#36rZZ")
      );
    "#16R11.8a">::
      (fun _ ->
        assert_equal_sexpr
          (Number (Float 17.5390625))
          (Reader.Reader.read_sexpr "#16R11.8a")
      );
    "#2r-1101">::
      (fun _ ->
        assert_equal_sexpr
          (Number (Int (-13)))
          (Reader.Reader.read_sexpr "#2r-1101")
      );
    "#calc_no_mant 2 1101">::
      (fun _ ->
        assert_equal
          ~printer: string_of_int
          13
          (Reader.calc_no_mant 2 ['1';'1';'0';'1'])
      );
    "#calc_mant 16 8a">::
      (fun _ ->
        assert_equal
          ~printer: string_of_float
          0.5390625
          (Reader.calc_mant 16. ['8';'a'])
      );
    "#calc_mant 10 12">::
      (fun _ ->
        assert_equal
          ~printer: string_of_float
          0.12
          (Reader.calc_mant 10. ['1';'2'])
      );
  ];;

let symbol_suite =
"symbol suite">:::
  [
    "abc">::
      (fun _ ->
        assert_equal_sexpr
          (Symbol "abc")
          (Reader.Reader.read_sexpr "abc")
      );
    "Abc">::
      (fun _ ->
        assert_equal_sexpr
          (Symbol "abc")
          (Reader.Reader.read_sexpr "Abc")
      );
    "ABC!$^*-_=+<>/?">::
      (fun _ ->
        assert_equal_sexpr
          (Symbol "abc!$^*-_=+<>/?")
          (Reader.Reader.read_sexpr "ABC!$^*-_=+<>/?")
      );
    "        ABCabc0123!$^*-_=+<>/?">::
      (fun _ ->
        assert_equal_sexpr
          (Symbol "abcabc0123!$^*-_=+<>/?")
          (Reader.Reader.read_sexpr "        abcabc0123!$^*-_=+<>/?     ")
      );
    "ABCabc0123!$^*-_=+<>/?#;abc">::
      (fun _ ->
        assert_equal_sexpr
          (Symbol "abcabc0123!$^*-_=+<>/?")
          (Reader.Reader.read_sexpr "abcabc0123!$^*-_=+<>/?#;abc")
      );
    "#;ABCabc0123!$^*-_=+<>/?  abc">::
      (fun _ ->
        assert_equal_sexpr
          (Symbol "abc")
          (Reader.Reader.read_sexpr "#;ABCabc0123!$^*-_=+<>/?  abc")
      );
    ";ABCabc0123!$^*-_=+<>/?\nabc">::
      (fun _ ->
        assert_equal_sexpr
          (Symbol "abc")
          (Reader.Reader.read_sexpr ";ABCabc0123!$^*-_=+<>/?\nabc")
      );    
    
  ];;

let references_suite =
"references suite">:::
  [
    "#{x}=(a.  #{x})">::
      (fun _ ->
        assert_equal_sexpr
          (TaggedSexpr ("x", Pair (Symbol "a", TagRef "x")))
          (Reader.Reader.read_sexpr "#{x}=(a.  #{x})")
      );
    "#{foo}=(#{foo}=1 2 3)">::
      (fun _ ->
        assert_raises
          Reader.X_this_should_not_happen
          (fun _->
            let _i = (Reader.Reader.read_sexpr "#{foo}=(#{foo}=1 2 3)") in
            ()
          )
      );
     "#{foo}=(#{foo}=1 2 3)">::
      (fun _ ->
        assert_raises
          Reader.X_this_should_not_happen
          (fun _->
            let _i = (Reader.Reader.read_sexpr "#{foo}=(#{foo}=1 2 3)") in
            ()
          )
      )
  ];;


let () =
  run_test_tt_main string_suite;
  run_test_tt_main char_suite;
  run_test_tt_main list_suite;
  run_test_tt_main qouted_forms_suite;
  run_test_tt_main number_suite;
  run_test_tt_main symbol_suite;
  run_test_tt_main references_suite;
;;
