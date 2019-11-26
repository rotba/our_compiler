# use "reader.ml";;

let assert_equal_sexpr sexpr1 sexpr2=
  let res  = sexpr_eq sexpr1 sexpr2 in
  let handle_eq = Printf.sprintf "%s" "." in
  let handle_not_eq = Printf.sprintf "fail\nexpected: %s\nbut got:%s" (sexpr_to_string sexpr1) (sexpr_to_string sexpr2) in
  if(res) then handle_eq else handle_not_eq;;


(assert_equal_sexpr (Char 'a') (Reader.read_sexpr "#\\a"));;
(assert_equal_sexpr (Char '\127') (Reader.read_sexpr "#\\\127"));;

 (* "out_of_range_128">:: (fun _ ->assert_raises
  *    Reader.PC.X_no_match
  *    (fun _ ->
  *      let res = (Reader.Reader.read_sexpr "#\\\128") in
  *      ())); *)
    (* "out_of_range_space">:: (fun _ -> assert_raises
     * Reader.PC.X_no_match
     * (fun _ ->
     *   let res = (Reader.read_sexpr "#\\\ ") in
     *   ())); *)
(assert_equal_sexpr (Char (Char.chr 10)) (Reader.read_sexpr "#\\newline"));;
(assert_equal_sexpr (Char (Char.chr 12)) (Reader.read_sexpr "#\\Page"));;
(assert_equal_sexpr (Char 'a') (Reader.read_sexpr "   #\\a    "));;
(assert_equal_sexpr (Char (Char.chr 12)) (Reader.read_sexpr ";this is a comment\n#\\Page"));;
(assert_equal_sexpr (Char (Char.chr 12)) (Reader.read_sexpr ";this is a comment\n;this is a comment\n#\\Page"));;
(assert_equal_sexpr (Char 'a') (Reader.read_sexpr "#;\"moshe\"#\\a"));;

(assert_equal_sexpr (String "moshe") (Reader.read_sexpr "\"moshe\""));;
(assert_equal_sexpr
    (String
       "This is a very longstring that don't spills acrossseveral lines."
    )
    (Reader.read_sexpr
       "\"This is a very longstring that don't spills acrossseveral lines.\""
));;
(assert_equal_sexpr
    (String
       "This is a very long\nstring that spills across\nseveral lines."
    )
    (Reader.read_sexpr
       "\"This is a very long\nstring that spills across\nseveral lines.\""
));;
(assert_equal_sexpr (String "") (Reader.read_sexpr "\"\""));;
(assert_equal_sexpr
    (String
       "This is a very long\nstring that spills across\nseveral lines."
    )
    (Reader.read_sexpr
       "\"This is a very long\Nstring that spills across\nseveral lines.\""
));;
(assert_equal_sexpr (String "moshe") (Reader.read_sexpr "   \"moshe\"    "));;
(assert_equal_sexpr (String "moshe") (Reader.read_sexpr ";this is a comment\n\"moshe\""));;
(assert_equal_sexpr (String "moshe") (Reader.read_sexpr ";this is a comment\n\"moshe\";this is also a comment"));;
(assert_equal_sexpr (String "moshe") (Reader.read_sexpr "#;#t\  #;#\\a \"moshe\""));;

(assert_equal_sexpr
   (Pair(Char 'a', Pair(Char 'b', Pair(Char 'c',Nil ))) )
   (Reader.read_sexpr "(#\\a #\\b #\\c)")
);;
(assert_equal_sexpr
   (Nil )
   (Reader.read_sexpr "()")
);;
(assert_equal_sexpr
   (Nil )
   (Reader.read_sexpr "(#;#T   ;asjdfasdkjsadjhka\n)")
);;
(assert_equal_sexpr
          (Pair(
               Char 'a',
               Pair(
                   Pair(Char 'b', Pair(Char 'c',Nil )),
                   Nil)
             )
          )
          (Reader.read_sexpr "(#\\a (#\\b #\\c))")
      );;
(assert_equal_sexpr
   Pair(
     Char 'a',
     Pair(Char 'b',
          Pair(Char 'c',Nil)
   ))
          (Reader.read_sexpr "(          #\\a     #\\b  #;#t #\\c  )")
      );;
(assert_equal_sexpr
   Pair(Char 'a',
        Pair(Char 'b', Char 'c')) 
          (Reader.read_sexpr "(#\\a #\\b .#\\c)")
      );;
(assert_equal_sexpr
          Pair(Char 'a', Pair(Char 'b', Char 'c')) 
          (Reader.read_sexpr "(          #\\a     #\\b  #;#t .#\\c  )")
      );;
(assert_equal_sexpr
   Pair(
     Char 'a',
               Pair(Char 'b', Pair(Char 'c',Nil ))
          )
          (Reader.read_sexpr "(#\\a .(#\\b #\\c))")
      );;     

(assert_equal_sexpr
          (Pair(Symbol("qoute"),Pair(Char 'a', Nil)))
          (Reader.read_sexpr "'#\\a")
      );;
(assert_equal_sexpr
          (Pair(
               Symbol("qoute"),
               Pair(
                   Pair(Char 'a', Pair(Char 'b', Pair(Char 'c',Nil ))),
                   Nil)
          ))
          (Reader.read_sexpr "'(#\\a #\\b #\\c)")
      );;
(assert_equal_sexpr
          (Pair(Symbol("unquote"),Pair(Char 'a', Nil)))
          (Reader.read_sexpr ",#\\a")
      );;
(assert_equal_sexpr
          (Pair(
               Symbol("unquote"),
               Pair(
                   Pair(Char 'a', Pair(Char 'b', Pair(Char 'c',Nil ))),
                   Nil)
          ))
          (Reader.read_sexpr ",(#\\a #\\b #\\c)")
      );;
(assert_equal_sexpr
          (Pair(Symbol("unquote-splicing"),Pair(Char 'a', Nil)))
          (Reader.read_sexpr ",@#\\a")
      );;
(assert_equal_sexpr
          (Pair(Symbol("unquote-splicing"),Pair(Bool true, Nil)))
          (Reader.read_sexpr ",@#t")
      );;


(assert_equal_sexpr
          (Number (Int 1))
          (Reader.read_sexpr "1")
      );;
(assert_equal_sexpr
          (Number (Int 1))
          (Reader.read_sexpr "+1")
      );;
(assert_equal_sexpr
          (Number (Int (-1)))
          (Reader.read_sexpr "-1")
      );;
(assert_equal_sexpr
          (Number (Int 1234))
          (Reader.read_sexpr "1234")
      );;
(assert_equal_sexpr
          (Number (Int 1234))
          (Reader.read_sexpr "01234")
      );;
(assert_equal_sexpr
          (Number (Int (-1234)))
          (Reader.read_sexpr "-01234")
      );;
(assert_equal_sexpr
          (Number (Int 1234))
          (Reader.read_sexpr "+01234")
      );;
(assert_equal_sexpr
          (Number (Int 0))
          (Reader.read_sexpr "-0")
      );;
(assert_equal_sexpr
          (Number (Float 5.0129))
          (Reader.read_sexpr "0005.0129")
      );;
(assert_equal_sexpr
          (Number (Float 501.1))
          (Reader.read_sexpr "501.100000000000000000000")
      );;
(assert_equal_sexpr
          (Number (Float 0.0))
          (Reader.read_sexpr "-0.0")
      );;
(assert_equal_sexpr
          (Number (Float 999.12349999999))
          (Reader.read_sexpr "+999.12349999999")
      );;
(assert_equal_sexpr
          (Number (Float 10.0))
          (Reader.read_sexpr "1e1")
      );;
(assert_equal_sexpr
          (Number (Float 10.0))
          (Reader.read_sexpr "1E+1")
      );;
(assert_equal_sexpr
          (Number (Float 1.0))
          (Reader.read_sexpr "10e-1")
      );;
(assert_equal_sexpr
          (Number (Float 3.14e+9))
          (Reader.read_sexpr "3.14e+9")
      );;
(assert_equal_sexpr
          (Number (Float 3.14E-512))
          (Reader.read_sexpr "3.14E-512")
      );;
(assert_equal_sexpr
          (Number (Float 1230.0))
          (Reader.read_sexpr "+000000012.3E00000002")
      );;
(assert_equal_sexpr
          (Number (Float (-0.05)))
          (Reader.read_sexpr "-5.000000000e-2")
      );;
(* (assert_raises
 *           Reader.PC.X_no_match
 *           (fun _->
 *             let _i = (Reader.read_sexpr "#-1r1") in
 *             ()
 *           )
 *       );; *)
(assert_equal_sexpr
          (Number (Int 1295))
          (Reader.read_sexpr "#36rZZ")
      );;
(assert_equal_sexpr
          (Number (Float 17.5390625))
          (Reader.read_sexpr "#16R11.8a")
      );;
(assert_equal_sexpr
          (Number (Int (-13)))
          (Reader.read_sexpr "#2r-1101")
      );;


(assert_equal_sexpr
          (Symbol "abc")
          (Reader.read_sexpr "abc")
      );;
(assert_equal_sexpr
          (Symbol "abc")
          (Reader.read_sexpr "Abc")
      );;
(assert_equal_sexpr
          (Symbol "abc!$^*-_=+<>/?")
          (Reader.read_sexpr "ABC!$^*-_=+<>/?")
      );;
(assert_equal_sexpr
          (Symbol "abcabc0123!$^*-_=+<>/?")
          (Reader.read_sexpr "        abcabc0123!$^*-_=+<>/?     ")
      );;
(assert_equal_sexpr
          (Symbol "abcabc0123!$^*-_=+<>/?")
          (Reader.read_sexpr "abcabc0123!$^*-_=+<>/?#;abc")
      );;
(assert_equal_sexpr
          (Symbol "abc")
          (Reader.read_sexpr "#;ABCabc0123!$^*-_=+<>/?  abc")
      );;
(assert_equal_sexpr
          (Symbol "abc")
          (Reader.read_sexpr ";ABCabc0123!$^*-_=+<>/?\nabc")
      );;
    
(assert_equal_sexpr
          (TaggedSexpr ("x", Pair (Symbol "a", TagRef "x")))
          (Reader.read_sexpr "#{x}=(a.  #{x})")
      );;
    (* "#{foo}=(#{foo}=1 2 3)">::
     *   (fun _ ->
     *     assert_raises
     *       Reader.X_this_should_not_happen
     *       (fun _->
     *         let _i = (Reader.read_sexpr "#{foo}=(#{foo}=1 2 3)") in
     *         ()
     *       )
     *   ); *)
     (* "#{foo}=(#{foo}=1 2 3)">::
      *  (fun _ ->
      *    assert_raises
      *      Reader.X_this_should_not_happen
      *      (fun _->
      *        let _i = (Reader.read_sexpr "#{foo}=(#{foo}=1 2 3)") in
      *        ()
      *      )
      *  ) *)
