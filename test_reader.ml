#use "topfind";;
#require "oUnit";;

#use "reader.ml";;

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

let assert_equal_sexpr sexpr1 sexpr2=
  assert_equal
    ~cmp: sexpr_eq
    ~printer:sexpr_to_string
    sexpr1
    sexpr2
  ;;


(* Name the test cases and group them together *)
let char_suite =
"char suite">:::
  [
    "a">::(fun _ -> assert_equal_sexpr (Char 'a') (read_sexpr "#\\a"));
    "esc">:: (fun _ ->assert_equal_sexpr (Char '\127') (Reader.read_sexpr "#\\\127"));
    "out_of_range_128">:: (fun _ ->assert_raises
    PC.X_no_match
    (fun _ ->
      let res = (Reader.read_sexpr "#\\\128") in
      ()));
    "out_of_range_space">:: (fun _ -> assert_raises
    PC.X_no_match
    (fun _ ->
      let res = (Reader.read_sexpr "#\\\ ") in
      ()));
    "newline">:: (fun _ ->assert_equal_sexpr (Char (Char.chr 10)) (Reader.read_sexpr "#\\newline"));
    "page in sensitive">:: (fun _ ->assert_equal_sexpr (Char (Char.chr 12)) (Reader.read_sexpr "#\\Page"));
    "with whitespaces">:: (fun _ ->assert_equal_sexpr (Char 'a') (Reader.read_sexpr "   #\\a    "));
    "with comment">:: (fun _ ->assert_equal_sexpr (Char (Char.chr 12)) (Reader.read_sexpr ";this is a comment\n#\\Page"));
    "with comments">:: (fun _ ->  assert_equal_sexpr (Char (Char.chr 12)) (Reader.read_sexpr ";this is a comment\n;this is a comment\n#\\Page"));
    "with sexpr comment">:: (fun _ -> assert_equal_sexpr (Char 'a') (Reader.read_sexpr "#;\"moshe\"#\\a"))
  ];;

let string_suite =
"string_suite">:::
  [
    "moshe">:: (fun _ -> assert_equal_sexpr (String "moshe") (Reader.read_sexpr "\"moshe\""));
    "long string">:: (fun _ -> assert_equal_sexpr
    (String
       "This is a very longstring that don't spills acrossseveral lines."
    )
    (Reader.read_sexpr
       "\"This is a very longstring that don't spills acrossseveral lines.\""
    ));
    "with meta">:: (fun _ ->assert_equal_sexpr
    (String
       "This is a very long\nstring that spills across\nseveral lines."
    )
    (Reader.read_sexpr
       "\"This is a very long\nstring that spills across\nseveral lines.\""
    ));
    "empty str">:: (fun _ -> assert_equal_sexpr (String "") (Reader.read_sexpr "\"\""));
    "with meta ci">:: (fun _ -> assert_equal_sexpr
    (String
       "This is a very long\nstring that spills across\nseveral lines."
    )
    (Reader.read_sexpr
       "\"This is a very long\Nstring that spills across\nseveral lines.\""
    ));
    "with whitespaces">:: (fun _ -> assert_equal_sexpr (String "moshe") (Reader.read_sexpr "   \"moshe\"    "));
    "with comment">:: (fun _ -> assert_equal_sexpr (String "moshe") (Reader.read_sexpr ";this is a comment\n\"moshe\""));
    "with comments">:: (fun _ -> assert_equal_sexpr (String "moshe") (Reader.read_sexpr ";this is a comment\n\"moshe\";this is also a comment"));
    "with nested sexpr comments">:: (fun _ -> assert_equal_sexpr (String "moshe") (Reader.read_sexpr "#;#t\  #;#\\a \"moshe\""))
  ];;

let list_suite =
"list_suite">:::
  [
    "(a b c)">::
      (fun _ ->
        assert_equal_sexpr
          (Pair(Char 'a', Pair(Char 'b', Pair(Char 'c',Nil ))) )
          (Reader.read_sexpr "(#\\a #\\b #\\c)")
      );
    "()">::
      (fun _ ->
        assert_equal_sexpr
          (Nil )
          (Reader.read_sexpr "()")
      );

    "(   )">::
      (fun _ ->
        assert_equal_sexpr
          (Nil )
          (Reader.read_sexpr "(    )")
      );
    "(#;#T   ;asjdfasdkjsadjhka\n)">::
      (fun _ ->
        assert_equal_sexpr
          (Nil )
          (Reader.read_sexpr "(#;#T   ;asjdfasdkjsadjhka\n)")
      );
    "(a (b c))">::
      (fun _ ->
        assert_equal_sexpr
          (Pair(
               Char 'a',
               Pair(
                   Pair(Char 'b', Pair(Char 'c',Nil )),
                   Nil)
             )
          )
          (Reader.read_sexpr "(#\\a (#\\b #\\c))")
      );
    "(a         b #;#t c)">::
      (fun _ ->
        assert_equal_sexpr
          (Pair(Char 'a', Pair(Char 'b', Pair(Char 'c',Nil))) )
          (Reader.read_sexpr "(          #\\a     #\\b  #;#t #\\c  )")
      );

     "(a         b #;#t .c)">::
      (fun _ ->
        assert_equal_sexpr
          (Pair(Char 'a', Pair(Char 'b', Char 'c')) )
          (Reader.read_sexpr "(#\\a #\\b .#\\c)")
      );
     "(a b . c)">::
      (fun _ ->
        assert_equal_sexpr
          (Pair(Char 'a', Pair(Char 'b', Char 'c')) )          
          (Reader.read_sexpr "(          #\\a     #\\b  #;#t .#\\c  )")
      );
    "(a .(b c))">::
      (fun _ ->
        assert_equal_sexpr
          (Pair(
               Char 'a',
               Pair(Char 'b', Pair(Char 'c',Nil ))
          ))
          (Reader.read_sexpr "(#\\a .(#\\b #\\c))")
      );     
  ];;

let qouted_forms_suite =
"qouted forms suite">:::
  [
    "'a">::
      (fun _ ->
        assert_equal_sexpr
          (Pair(Symbol("quote"),Pair(Char 'a', Nil)))
          (Reader.read_sexpr "'#\\a")
      );
    "'(a b c)">::
      (fun _ ->
        assert_equal_sexpr
          (Pair(
               Symbol("quote"),
               Pair(
                   Pair(Char 'a', Pair(Char 'b', Pair(Char 'c',Nil ))),
                   Nil)
          ))
          (Reader.read_sexpr "'(#\\a #\\b #\\c)")
      );
        ",a">::
      (fun _ ->
        assert_equal_sexpr
          (Pair(Symbol("unquote"),Pair(Char 'a', Nil)))
          (Reader.read_sexpr ",#\\a")
      );
    ",(a b c)">::
      (fun _ ->
        assert_equal_sexpr
          (Pair(
               Symbol("unquote"),
               Pair(
                   Pair(Char 'a', Pair(Char 'b', Pair(Char 'c',Nil ))),
                   Nil)
          ))
          (Reader.read_sexpr ",(#\\a #\\b #\\c)")
      );
    ",@a">::
      (fun _ ->
        assert_equal_sexpr
          (Pair(Symbol("unquote-splicing"),Pair(Char 'a', Nil)))
          (Reader.read_sexpr ",@#\\a")
      );
     ",@#t">::
      (fun _ ->
        assert_equal_sexpr
          (Pair(Symbol("unquote-splicing"),Pair(Bool true, Nil)))
          (Reader.read_sexpr ",@#t")
      );
  ];;


let number_suite =
"number suite">:::
  [
    "1">::
      (fun _ ->
        assert_equal_sexpr
          (Number (Int 1))
          (Reader.read_sexpr "1")
      );
    "+1">::
      (fun _ ->
        assert_equal_sexpr
          (Number (Int 1))
          (Reader.read_sexpr "+1")
      );
    "-1">::
      (fun _ ->
        assert_equal_sexpr
          (Number (Int (-1)))
          (Reader.read_sexpr "-1")
      );
    "1234">::
      (fun _ ->
        assert_equal_sexpr
          (Number (Int 1234))
          (Reader.read_sexpr "1234")
      );
    "01234">::
      (fun _ ->
        assert_equal_sexpr
          (Number (Int 1234))
          (Reader.read_sexpr "01234")
      );
    "-01234">::
      (fun _ ->
        assert_equal_sexpr
          (Number (Int (-1234)))
          (Reader.read_sexpr "-01234")
      );
    "+01234">::
      (fun _ ->
        assert_equal_sexpr
          (Number (Int 1234))
          (Reader.read_sexpr "+01234")
      );
    "-0">::
      (fun _ ->
        assert_equal_sexpr
          (Number (Int 0))
          (Reader.read_sexpr "-0")
      );
    "0005.0129">::
      (fun _ ->
        assert_equal_sexpr
          (Number (Float 5.0129))
          (Reader.read_sexpr "0005.0129")
      );
    "501.100000000000000000000">::
      (fun _ ->
        assert_equal_sexpr
          (Number (Float 501.1))
          (Reader.read_sexpr "501.100000000000000000000")
      );
    "-0.0">::
      (fun _ ->
        assert_equal_sexpr
          (Number (Float 0.0))
          (Reader.read_sexpr "-0.0")
      );
    "+999.12349999999">::
      (fun _ ->
        assert_equal_sexpr
          (Number (Float 999.12349999999))
          (Reader.read_sexpr "+999.12349999999")
      );
    "1e1">::
      (fun _ ->
        assert_equal_sexpr
          (Number (Float 10.0))
          (Reader.read_sexpr "1e1")
      );
    "1E+1">::
      (fun _ ->
        assert_equal_sexpr
          (Number (Float 10.0))
          (Reader.read_sexpr "1E+1")
      );
    "10e-1">::
      (fun _ ->
        assert_equal_sexpr
          (Number (Float 1.0))
          (Reader.read_sexpr "10e-1")
      );
    "3.14e+9">::
      (fun _ ->
        assert_equal_sexpr
          (Number (Float 3.14e+9))
          (Reader.read_sexpr "3.14e+9")
      );
    "3.14E-512">::
      (fun _ ->
        assert_equal_sexpr
          (Number (Float 3.14E-512))
          (Reader.read_sexpr "3.14E-512")
      );
    "+000000012.3E00000002">::
      (fun _ ->
        assert_equal_sexpr
          (Number (Float 1230.0))
          (Reader.read_sexpr "+000000012.3E00000002")
      );
    "-5.000000000e-2">::
      (fun _ ->
        assert_equal_sexpr
          (Number (Float (-0.05)))
          (Reader.read_sexpr "-5.000000000e-2")
      );
    "#-1r1">::
      (fun _ ->
        assert_raises
          PC.X_no_match
          (fun _->
            let _i = (Reader.read_sexpr "#-1r1") in
            ()
          )
      );
    "#36rZZ">::
      (fun _ ->
        assert_equal_sexpr
          (Number (Int 1295))
          (Reader.read_sexpr "#36rZZ")
      );
    "#16R11.8a">::
      (fun _ ->
        assert_equal_sexpr
          (Number (Float 17.5390625))
          (Reader.read_sexpr "#16R11.8a")
      );
    "#2r-1101">::
      (fun _ ->
        assert_equal_sexpr
          (Number (Int (-13)))
          (Reader.read_sexpr "#2r-1101")
      );
    "#calc_no_mant 2 1101">::
      (fun _ ->
        assert_equal
          ~printer: string_of_int
          13
          (calc_no_mant 2 ['1';'1';'0';'1'])
      );
    "#calc_mant 16 8a">::
      (fun _ ->
        assert_equal
          ~printer: string_of_float
          0.5390625
          (calc_mant 16. ['8';'a'])
      );
    "#calc_mant 10 12">::
      (fun _ ->
        assert_equal
          ~printer: string_of_float
          0.12
          (calc_mant 10. ['1';'2'])
      );
  ];;

let symbol_suite =
"symbol suite">:::
  [
    "abc">::
      (fun _ ->
        assert_equal_sexpr
          (Symbol "abc")
          (Reader.read_sexpr "abc")
      );
    "Abc">::
      (fun _ ->
        assert_equal_sexpr
          (Symbol "abc")
          (Reader.read_sexpr "Abc")
      );
    "ABC!$^*-_=+<>/?">::
      (fun _ ->
        assert_equal_sexpr
          (Symbol "abc!$^*-_=+<>/?")
          (Reader.read_sexpr "ABC!$^*-_=+<>/?")
      );
    "        ABCabc0123!$^*-_=+<>/?">::
      (fun _ ->
        assert_equal_sexpr
          (Symbol "abcabc0123!$^*-_=+<>/?")
          (Reader.read_sexpr "        abcabc0123!$^*-_=+<>/?     ")
      );
    "ABCabc0123!$^*-_=+<>/?#;abc">::
      (fun _ ->
        assert_equal_sexpr
          (Symbol "abcabc0123!$^*-_=+<>/?")
          (Reader.read_sexpr "abcabc0123!$^*-_=+<>/?#;abc")
      );
    "#;ABCabc0123!$^*-_=+<>/?  abc">::
      (fun _ ->
        assert_equal_sexpr
          (Symbol "abc")
          (Reader.read_sexpr "#;ABCabc0123!$^*-_=+<>/?  abc")
      );
    ";ABCabc0123!$^*-_=+<>/?\nabc">::
      (fun _ ->
        assert_equal_sexpr
          (Symbol "abc")
          (Reader.read_sexpr ";ABCabc0123!$^*-_=+<>/?\nabc")
      );    
    
  ];;

let references_suite =
"references suite">:::
  [
    "#{x}=(a.  #{x})">::
      (fun _ ->
        assert_equal_sexpr
          (TaggedSexpr ("x", Pair (Symbol "a", TagRef "x")))
          (Reader.read_sexpr "#{x}=(a.  #{x})")
      );
    "#{foo}=(#{foo}=1 2 3)">::
      (fun _ ->
        assert_raises
          X_this_should_not_happen
          (fun _->
            let _i = (Reader.read_sexpr "#{foo}=(#{foo}=1 2 3)") in
            ()
          )
      );
     "#{foo}=(#{foo}=1 2 3)">::
      (fun _ ->
        assert_raises
          X_this_should_not_happen
          (fun _->
            let _i = (Reader.read_sexpr "#{foo}=(#{foo}=1 2 3)") in
            ()
          )
      );
    "(1 #{foo}=2 3 #{foo}=4)">::
      (fun _ ->
        assert_raises
          X_this_should_not_happen
          (fun _->
            let _i = (Reader.read_sexpr "(1 #{foo}=2 3 #{foo}=4)") in
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
