#use "topfind";;
#require "oUnit";;

#use "tag-parser.ml";;


open Tag_Parser;;
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
  |TaggedSexpr(s,e) -> String.concat " , "["TaggedSexpr("; s;  (sexpr_to_string e) ;")"]
  |TagRef(x)->x
  |_ -> "not_implemented";;


  
let rec exp_to_string  =
  function
  |Const(Sexpr(s)) -> String.concat "" ["Const ( Sexpr ( "; (sexpr_to_string s); " ) )"]
  |Var(v) -> v
  |Set(exp1,exp2) -> String.concat " " ["(set!"; (exp_to_string exp1); (exp_to_string exp2);")"]
  |Seq(list) -> String.concat "" ["Seq([";(seq_to_string list exp_to_string);"])"]
  |Applic(name,params) -> String.concat "" ["Applic( "; (exp_to_string name);" , ["; (seq_to_string params exp_to_string)  ;"] )"]
  |LambdaSimple(params,body) -> String.concat "" ["(LambdaSimple( ";" ( " ; (string_list_to_string params ); " ) " ; ", ["; (exp_to_string body)  ;"] )"]
  |If(e1,e2,e3)-> String.concat "" ["(If( "; (exp_to_string e1 ); " , " ; (exp_to_string e2) ; "," ; (exp_to_string e3)  ;" )"]
  |_->"not_implemented"
    
and seq_to_string params to_string=
  let rec aggregate = function
  |[] -> []
  |f::r -> (to_string f) :: (aggregate r) in
  let params = (aggregate params) in
  String.concat " , " params

and string_list_to_string params=
  let rec aggregate = function
  |[] -> []
  |f::r -> f :: (aggregate r) in
  let params = (aggregate params) in
  String.concat " , " params
  
;;


let assert_equal_expr expr1 expr2=
  assert_equal
    ~cmp: expr_eq
    ~printer:exp_to_string
    expr1
    expr2
  ;;


(* Name the test cases and group them together *)
let simple_suite =
"simple suite">:::
  [
    "5">::(fun _ ->
      assert_equal_expr
        (Const (Sexpr (Number (Int 5))))
        (tag_parse_expression (Number  (Int 5))));
    "moshe">::(fun _ ->
      assert_equal_expr
        (Const (Sexpr (String "moshe")))
        (tag_parse_expression (String  "moshe")));
    "var xyz">::(fun _ ->
      assert_equal_expr
        (Var "xyz")
        (tag_parse_expression (Symbol  "xyz")));
    "var let">::
      (fun _ ->
        assert_raises
          X_syntax_error
          (fun _->
            let _i =(tag_parse_expression (Symbol  "let")) in
            ()
          )
      );
    "(or x y z)">::(fun _ ->
      assert_equal_expr
        (Or [(Var "x");(Var "y");(Var "z")])
        (tag_parse_expression
           (Pair(
                Symbol("or"),
                Pair(Symbol("x"),Pair(Symbol("y"),Pair(Symbol("z"),Nil)))
              )
           )
        )
    );
    "(set! x 1)">::(fun _ ->
      assert_equal_expr
        (Set(Var("x"),Const(Sexpr(Number(Int(1))))))
        (tag_parse_expression
           (Pair(
                Symbol("set!"),
                Pair(Symbol("x"),Pair(Number(Int(1)),Nil))
              )
           )
        )
    );
  ];;

let less_simple_suite =
    "less simple suite">:::
      [
        "tagged def special case">::(fun _ ->
          assert_equal_expr
          (Const(Sexpr (TaggedSexpr ("x", Nil))))
          (tag_parse_expression (TaggedSexpr ("x", Pair (Symbol "quote", Pair (Nil, Nil)))))
        );
        "tag ref">::(fun _ ->
          assert_equal_expr
          (Const(Sexpr (TagRef "x")))
          (tag_parse_expression (TagRef "x"))
        );
        "1.(a)">::(fun _ ->
          assert_equal_expr
            (Const(Sexpr (TaggedSexpr ("x", Pair(Symbol "quote",Pair (Nil, Nil))))))
            (tag_parse_expression (Pair(Symbol "quote", Pair(TaggedSexpr("x", Pair (Symbol "quote", Pair (Nil, Nil))), Nil))))
        );
        "(lambda (x) x)">::(fun _ ->
          assert_equal_expr
            (LambdaSimple(["x"],Var("x")))
            (tag_parse_expression
               (Pair(
                     Symbol("lambda"),
                     Pair(
                         Pair(Symbol("x"),Nil),
                         Pair(Symbol("x"),Nil)
                       )
                  )
               )
            )
        );
        "(lambda () x)">::(fun _ ->
          assert_equal_expr
            (LambdaSimple([],Var("x")))
            (tag_parse_expression
               (Pair(
                     Symbol("lambda"),
                     Pair(
                         Nil,
                         Pair(Symbol("x"),Nil)
                       )
                  )
               )
            )
        );
        "(lambda (x . y) x)">::(fun _ ->
          assert_equal_expr
            (LambdaOpt(["x"], "y", Var("x")))
            (tag_parse_expression
               (Pair(
                     Symbol("lambda"),
                     Pair(
                         Pair(Symbol("x"),Symbol("y")),
                         Pair(Symbol("x"),Nil)
                       )
                  )
               )
            )
        );
        "(lambda (x . y) x)">::(fun _ ->
          assert_equal_expr
            (Applic(Var ("asdf"),[]) )
            (tag_parse_expression
               (Pair(
                     Symbol("asdf"),
                     Nil
                  )
               )
            )
        );
        "(lambda () x)">::(fun _ ->
          assert_equal_expr
            (Applic(Var ("asdf"),[]) )
            (tag_parse_expression
               (Pair(
                     Symbol("asdf"),
                     Nil
                  )
               )
            )
        );
      ];;

let qq =
    "quasiquoate">:::
      [
        "`,x">::(fun _ ->
          assert_equal_expr
          (Var("x"))
          (tag_parse_expression (
               Reader.read_sexpr("`,x")
             )
          )
        );
        "`()">::(fun _ ->
          assert_equal_expr
          (Const(Sexpr(Nil)))
          (tag_parse_expression (Reader.read_sexpr "`()"))
        );
        (* "`5">::(fun _ ->
       * assert_equal_expr
       *   (Const (Sexpr (Number (Int 5))))
       *   (tag_parse_expression (Reader.read_sexpr "`5"))); *)
        "`,@x">::
      (fun _ ->
        assert_raises
          X_syntax_error
          (fun _->
            let _i =
              (tag_parse_expression (
               Pair(
                   Symbol("quasiquote"),
                   Pair(
                       Symbol("unquote-splicing"),
                       Pair(Symbol("x"), Nil)
                     )
                 )
                 )
              )
            in
            ()
          )
      );
        "`(a b)">::(fun _ ->
          assert_equal_expr
            (
              Applic(
                  Var("cons"),
                      [
                        Const ( Sexpr ( Symbol("a") ) );
                        Applic(
                            Var("cons"),
                                [
                                  Const ( Sexpr ( Symbol("b") ) );
                                  Const ( Sexpr ( Nil ) )
                                ]
                          )
                      ]
                )
            )
          (tag_parse_expression (
               Reader.read_sexpr("`(a b)")
             )
          )
        );
        "`(,@a b)">::(fun _ ->
          assert_equal_expr
            (
              Applic(
                  Var("append"),
                      [
                        Var("a");
                        Applic(
                            Var("cons"),
                                [
                                  Const ( Sexpr ( Symbol("b") ) );
                                  Const ( Sexpr ( Nil ) )
                                ]
                          )
                      ]
                )
            )
          (tag_parse_expression (
               Reader.read_sexpr("`(,@a b)")
             )
          )
        );
        "`(a . ,@b)">::(fun _ ->
          assert_equal_expr
            (
              Applic(
                  Var("cons"),
                      [
                        Const ( Sexpr ( Symbol("a") ) );
                        Var("b")
                      ]
                )
            )
          (tag_parse_expression (
               Reader.read_sexpr("`(a . ,@b)")
             )
          )
        );
      ];;

      let cond =
        "cond">:::
          [
            "cond test">::(fun _ ->
              assert_equal_expr
              (
                If(
                  Applic(
                    Var("zero?"),
                    [Var("n")]
                  ),
                  Seq(
                    [
                      Applic(Var("f"), [Var("x")]);
                      Applic(Var("g"), [Var("y")])
                    ]
                  ),
                  Seq(
                    [
                      Applic(Var("h"), [Var("x");Var("y")]);
                      Applic(Var("g"), [Var("x")])
                    ]
                  )
                )
              )
              (tag_parse_expression (
                   Reader.read_sexpr("(cond ((zero? n) (f x) (g y)) (else (h x y) (g x)) ((q? y) (p x) (q y)))")
              )
            ));
          ];;
let lets =
    "lets">:::
      [
        "(let ((x 1)) x)">::(fun _ ->
          assert_equal_expr
            (Applic(
              LambdaSimple(
                  ["x"],
                  Var("x")
                ),
              [Const(Sexpr(Number(Int(1))))]
            ))
            (tag_parse_expression (Reader.read_sexpr("(let ((x 1)) x)")) )
        );
        "(let () 1)">::(fun _ ->
          assert_equal_expr
            (Applic(
              LambdaSimple(
                  [],
                  Const(Sexpr(Number(Int(1))))
                ),
              []
            ))
            (tag_parse_expression (Reader.read_sexpr("(let () 1)")) )
        );
        "(let* ((x 1)) x)">::(fun _ ->
          assert_equal_expr
            (Applic(
              LambdaSimple(
                  ["x"],
                  Var("x")
                ),
              [Const(Sexpr(Number(Int(1))))]
            ))
            (tag_parse_expression (Reader.read_sexpr("(let* ((x 1)) x)")) )
        );
        "(let* () x)">::(fun _ ->
          assert_equal_expr
            (Applic(
              LambdaSimple(
                  [],
                  Var("x")
                ),
              []
            ))
            (tag_parse_expression (Reader.read_sexpr("(let* () x)")) )
        );
        "(let ((x 1) (y x)) x)">::(fun _ ->
          assert_equal_expr
            (Applic(
              LambdaSimple(
                  ["x";"y"],
                  Var("x")
                ),
              [Const(Sexpr(Number(Int(1)))); Var("x")]
            ))
            (tag_parse_expression (Reader.read_sexpr("(let ((x 1) (y x)) x)")) )
        );
        "(let* ((x 1) (y x)) x)">::(fun _ ->
          assert_equal_expr
            (Applic(
              LambdaSimple(
                  ["x"],
                  Applic(
                      LambdaSimple(
                          ["y"],
                          Var("x")
                        ),
                      [Var("x")]
                    )
                ),
              [Const(Sexpr(Number(Int(1))))]
            ))
            (tag_parse_expression (Reader.read_sexpr("(let* ((x 1) (y x)) x)")) )
        );
        "(letrec ((x 1)) x)">::(fun _ ->
          assert_equal_expr
            (Applic(
              LambdaSimple(
                  ["x"],
                  Seq([
                        Set(Var("x"), Const(Sexpr(Number(Int(1)))));
                        Var("x")
                    ])
                  
                ),
              [Const(Sexpr(Symbol("whatever")))]
            ))
            (tag_parse_expression (Reader.read_sexpr("(letrec ((x 1)) x)")) )
        );
        "test_tag_let_rec_expression_parser_2">::(fun _ ->
          assert_equal_expr
            (Applic
                                                                   (LambdaSimple (["fact"],
                                                                     Seq
                                                                      [Set (Var "fact",
                                                                        LambdaSimple (["n"],
                                                                         If (Applic (Var "zero?", [Var "n"]), Const (Sexpr (Number (Int 1))),
                                                                          Applic (Var "*",
                                                                           [Var "n";
                                                                            Applic (Var "fact",
                                                                             [Applic (Var "-", [Var "n"; Const (Sexpr (Number (Int 1)))])])]))));
                                                                       Applic (Var "fact", [Const (Sexpr (Number (Int 5)))])]),
                                                                   [Const (Sexpr (Symbol "whatever"))])
                                                                  )
                      (Tag_Parser.tag_parse_expression (Pair (Symbol "letrec",
                                                         Pair
                                                          (Pair
                                                            (Pair (Symbol "fact",
                                                              Pair
                                                               (Pair (Symbol "lambda",
                                                                 Pair (Pair (Symbol "n", Nil),
                                                                  Pair
                                                                   (Pair (Symbol "if",
                                                                     Pair (Pair (Symbol "zero?", Pair (Symbol "n", Nil)),
                                                                      Pair (Number (Int 1),
                                                                       Pair
                                                                        (Pair (Symbol "*",
                                                                          Pair (Symbol "n",
                                                                           Pair
                                                                            (Pair (Symbol "fact",
                                                                              Pair
                                                                               (Pair (Symbol "-",
                                                                                 Pair (Symbol "n", Pair (Number (Int 1), Nil))),
                                                                               Nil)),
                                                                            Nil))),
                                                                        Nil)))),
                                                                   Nil))),
                                                               Nil)),
                                                            Nil),
                                                          Pair (Pair (Symbol "fact", Pair (Number (Int 5), Nil)), Nil)))
                                                        ))
        );
      ];;



let () =
  run_test_tt_main simple_suite;
  run_test_tt_main less_simple_suite;
  run_test_tt_main cond;
  run_test_tt_main qq;
  run_test_tt_main lets;
;;
