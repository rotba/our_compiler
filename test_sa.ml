#use "topfind";;
#require "oUnit";;

#use "semantic-analyser.ml";;


open Semantics;;
open Tag_Parser;;
open Reader;;
open OUnit2;;



let assert_equal_expr_tag expr1 expr2=
  assert_equal
    (* ~cmp: expr'_eq *)
    ~printer: exp'_to_string
    expr1
    expr2
  ;;


(* Name the test cases and group them together *)
let simple_suite =
"simple suite">:::
  [
    "(lambda(x) (x (lambda (y)(x y (lambda(z) (x y z))))))">::(fun _ ->
      assert_equal_expr_tag
        (LambdaSimple'(
             ["x"],
             Applic'(
                 Var'(VarParam("x",0)),
                 [
                   LambdaSimple'(
                       ["y"],
                       Applic'(
                           Var'(VarBound("x",0,0)),
                           [
                             Var'(VarParam("y",0));
                             LambdaSimple'(
                                 ["z"],
                                 Applic'(
                                     Var'(VarBound("x",1,0)),
                                     [
                                       Var'(VarBound("y",0,0));
                                       Var'(VarParam("z",0))
                                     ]
                                   )
                               )
                           ]
                         )
                     )
                 ]
               )
        ))
        (Semantics.annotate_lexical_addresses(Tag_Parser.tag_parse_expression(
             Reader.read_sexpr(
                 "(lambda(x) (x (lambda (y)(x y (lambda(z) (x y z))))))"
           )))
        )
    );
    "(lambda (x) x)">::(fun _ ->
      assert_equal_expr_tag
        (LambdaSimple'(
             ["x"],
             Var'(VarParam("x", 0))
        ))
        (Semantics.annotate_lexical_addresses(Tag_Parser.tag_parse_expression(
             Reader.read_sexpr(
                 "(lambda(x) x)"
           )))
        )
    )
  ];;


let last_year =
"last year">:::
  [
    "_0">::(fun _ ->
      assert_equal_expr_tag
        (
          Applic' (LambdaSimple' (["x"], If' (Applic' (Var' (VarParam ("x", 0)), [Const' (Sexpr (Number (Int (1))))]), Applic' (Var' (VarParam ("x", 0)), [Const' (Sexpr (Number (Int (2))))]), Applic' (LambdaSimple' (["x"], Set' (Var' (VarParam ("x", 0)), Const' (Sexpr (Number (Int (0)))))), [Const' (Sexpr (Number (Int (3))))]))), [LambdaSimple' (["x"], Var' (VarParam ("x", 0)))])
        )
        (Semantics.annotate_lexical_addresses(
             Applic
               (LambdaSimple (["x"],
                              If (Applic (Var "x", [Const (Sexpr (Number (Int 1)))]),
                                  Applic (Var "x", [Const (Sexpr (Number (Int 2)))]),
                                  Applic
                                    (LambdaSimple (["x"], Set (Var "x", Const (Sexpr (Number (Int 0))))),
                                     [Const (Sexpr (Number (Int 3)))]))),
                [LambdaSimple (["x"], Var "x")])
           )
        )
    );
    "_1">::(fun _ ->
      assert_equal_expr_tag
        (
          Applic' (LambdaSimple' (["x"], If' (Applic' (Var' (VarParam ("x", 0)), [Const' (Sexpr (Number (Int (1))))]), Applic' (Var' (VarParam ("x", 0)), [Const' (Sexpr (Number (Int (2))))]), Applic' (LambdaSimple' (["x"], Set' (Var' (VarParam ("x", 0)), Const' (Sexpr (Number (Int (0)))))), [Const' (Sexpr (Number (Int (3))))]))), [LambdaSimple' (["x"], Var' (VarParam ("x", 0)))])
        )
        (Semantics.annotate_lexical_addresses(
             Applic
               (LambdaSimple (["x"],
                              If (Applic (Var "x", [Const (Sexpr (Number (Int 1)))]),
                                  Applic (Var "x", [Const (Sexpr (Number (Int 2)))]),
                                  Applic
                                    (LambdaSimple (["x"], Set (Var "x", Const (Sexpr (Number (Int 0))))),
                                     [Const (Sexpr (Number (Int 3)))]))),
                [LambdaSimple (["x"], Var "x")])
           )
        )
    );
    "_2">::(fun _ ->
      assert_equal_expr_tag
        (
          LambdaSimple' (["x"], Or' ([Applic' (LambdaOpt' (["y"], "z", Applic' (LambdaSimple' ([], Applic' (LambdaSimple' ([], Applic' (Var' (VarFree "+"), [Var' (VarBound ("x", 2, 0));Var' (VarBound ("z", 1, 1))])), [])), [])), [Var' (VarParam ("x", 0));Const' (Sexpr (Number (Int (1))))]);LambdaSimple' ([], Set' (Var' (VarBound ("x", 0, 0)), Var' (VarFree "w")));Applic' (Var' (VarFree "w"), [Var' (VarFree "w")])]))
        )
        (Semantics.annotate_lexical_addresses(
             LambdaSimple (["x"],
                           Or
                             [Applic
                                (LambdaOpt (["y"], "z",
                                            Applic
                                              (LambdaSimple ([],
                                                             Applic (LambdaSimple ([], Applic (Var "+", [Var "x"; Var "z"])), [])),
                                               [])),
                                 [Var "x"; Const (Sexpr (Number (Int 1)))]);
                              LambdaSimple ([], Set (Var "x", Var "w")); Applic (Var "w", [Var "w"])])
           )
        )
    );
    "_3">::(fun _ ->
      assert_equal_expr_tag
        (
          If' (Applic' (LambdaSimple' (["y"], Var' (VarFree "x")), []), Applic' (LambdaSimple' (["x"], Seq' ([Set' (Var' (VarParam ("x", 0)), Var' (VarFree "y"));LambdaSimple' ([], Set' (Var' (VarBound ("x", 0, 0)), Const' (Sexpr (Number (Int (1))))))])), [Const' (Sexpr (Symbol "a"))]), LambdaSimple' (["x"], Set' (Var' (VarParam ("x", 0)), Var' (VarFree "y"))))
        )
        (Semantics.annotate_lexical_addresses(
             If (Applic (LambdaSimple (["y"], Var "x"), []),
                 Applic
                   (LambdaSimple (["x"],
                                  Seq
                                    [Set (Var "x", Var "y");
                                     LambdaSimple ([], Set (Var "x", Const (Sexpr (Number (Int 1)))))]),
                    [Const (Sexpr (Symbol "a"))]),
                 LambdaSimple (["x"], Set (Var "x", Var "y")))
           )
        )
    );
    "_4">::(fun _ ->
      assert_equal_expr_tag
        (
          Def' (Var' (VarFree "a"), Applic' (LambdaSimple' ([], LambdaOpt' ([], "x", Seq' ([Var' (VarParam ("x", 0));LambdaOpt' ([], "y", Set' (Var' (VarParam ("y", 0)), Const' (Sexpr (Number (Int (1))))))]))), []))
        )
        (Semantics.annotate_lexical_addresses(
             Def (Var "a",
                  Applic
                    (LambdaSimple ([],
                                   LambdaOpt ([], "x",
                                              Seq
                                                [Var "x";
                                                 LambdaOpt ([], "y", Set (Var "y", Const (Sexpr (Number (Int 1)))))])),
                     []))
           )
        )
    );
  ];;

let boxing =
  "boxing">:::
    [
      "foo_should_be_boxed">::(fun _ ->
        assert_equal_expr_tag
          (
            Def' (Var'(VarFree("foo")),
                  LambdaSimple'(
                      ["x"],
                      Seq'(
                          [
                            Set'(Var'(VarParam("x",0)), Box'(VarParam("x",0)))
                          ;
                            ApplicTP'(
                                Var'(VarFree("list")),
                                [
                                  LambdaSimple'([],BoxGet'(VarBound("x", 0, 0)));
                                  LambdaSimple'(
                                      ["y"],
                                      BoxSet'(VarBound("x", 0, 0),Var'(VarParam("y", 0)))
                                    )
                                ]
                              )
                          ]
                        )

                    )
              )
          )
          (Semantics.box_set(
               Def' (Var'(VarFree("foo")),
                     LambdaSimple'(
                         ["x"],
                         ApplicTP'(
                             Var'(VarFree("list")),
                             [
                               LambdaSimple'([],Var'(VarBound("x", 0, 0)));
                               LambdaSimple'(
                                   ["y"],
                                   Set'(Var'(VarBound("x", 0, 0)),Var'(VarParam("y", 0)))
                                 )
                             ]
                           )
                       )
                 )
             )
          )
      );
      "foo2_should_not_be_boxed">::(fun _ ->
        assert_equal_expr_tag
          (
            Def' (Var'(VarFree("foo2")),
                  LambdaSimple'(
                      ["x"; "y"],
                      Set'(
                          Var'(VarParam("x", 0)),
                          Applic'(
                              Var'(VarFree("*")),
                              [Var'(VarParam("x", 0));Var'(VarParam("y", 1))]
                            )
                        )
                    )
              )
          )
          (Semantics.run_semantics(
               Tag_Parser.tag_parse_expression(
                   Reader.read_sexpr(
                       "(define foo2 (lambda (x y) (set! x (* x y))))"
                     )
                 )
             )
          )
      );
      "last_year 1">::(fun _ ->
        assert_equal_expr_tag
          (
            LambdaSimple' ([], Const' (Sexpr (Number (Int (1)))))
          )
          (Semantics.run_semantics(
               LambdaSimple ([], Const (Sexpr (Number (Int 1))))
             )
          )
      );
      "last_year 2">::(fun _ ->
        assert_equal_expr_tag
          (
            Applic' (
                LambdaSimple' (["x"],
                               If' (
                                   Applic' (
                                       Var' (VarParam ("x", 0)),
                                       [Const' (Sexpr (Number (Int (1))))]
                                     ),
                                   ApplicTP' (
                                       Var' (VarParam ("x", 0)),
                                       [Const' (Sexpr (Number (Int (2))))]
                                     ),
                                   ApplicTP' (
                                       LambdaSimple' (["x"],Set' (Var' (VarParam ("x", 0)), Const' (Sexpr (Number (Int (0))))))
                                     , [Const' (Sexpr (Number (Int (3))))]
                                     )
                                 )
                  )
              , [LambdaSimple' (["x"], Var' (VarParam ("x", 0)))])
          )
          (Semantics.run_semantics(
               Applic
                 (LambdaSimple (["x"],
                                If (Applic (Var "x", [Const (Sexpr (Number (Int 1)))]),
                                    Applic (Var "x", [Const (Sexpr (Number (Int 2)))]),
                                    Applic
                                      (LambdaSimple (["x"], Set (Var "x", Const (Sexpr (Number (Int 0))))),
                                       [Const (Sexpr (Number (Int 3)))]))),
                  [LambdaSimple (["x"], Var "x")])
             )
          )
      );
      "last_year_3">::(fun _ ->
        assert_equal_expr_tag
          (
            LambdaSimple' (["x"], Seq' ([Set' (Var' (VarParam ("x", 0)), Box' (VarParam ("x", 0)));Or' ([Applic' (LambdaOpt' (["y"], "z", ApplicTP' (LambdaSimple' ([], ApplicTP' (LambdaSimple' ([], ApplicTP' (Var' (VarFree "+"), [BoxGet' (VarBound ("x", 2, 0));Var' (VarBound ("z", 1, 1))])), [])), [])), [BoxGet' (VarParam ("x", 0));Const' (Sexpr (Number (Int (1))))]);LambdaSimple' ([], BoxSet' (VarBound ("x", 0, 0), Var' (VarFree "w")));ApplicTP' (Var' (VarFree "w"), [Var' (VarFree "w")])])]))
          )
          (Semantics.run_semantics(
               LambdaSimple (["x"],
                             Or
                               [Applic
                                  (LambdaOpt (["y"], "z",
                                              Applic
                                                (LambdaSimple ([],
                                                               Applic (LambdaSimple ([], Applic (Var "+", [Var "x"; Var "z"])), [])),
                                                 [])),
                                   [Var "x"; Const (Sexpr (Number (Int 1)))]);
                                LambdaSimple ([], Set (Var "x", Var "w")); Applic (Var "w", [Var "w"])])
             )
          )
      );
      "last_year_4">::(fun _ ->
        assert_equal_expr_tag
          (
            If' (Applic' (LambdaSimple' (["y"], Var' (VarFree "x")), []), Applic' (LambdaSimple' (["x"], Seq' ([Set' (Var' (VarParam ("x", 0)), Var' (VarFree "y"));LambdaSimple' ([], Set' (Var' (VarBound ("x", 0, 0)), Const' (Sexpr (Number (Int (1))))))])), [Const' (Sexpr (Symbol "a"))]), LambdaSimple' (["x"], Set' (Var' (VarParam ("x", 0)), Var' (VarFree "y"))))
          )
          (Semantics.run_semantics(
               If (Applic (LambdaSimple (["y"], Var "x"), []),
                   Applic
                     (LambdaSimple (["x"],
                                    Seq
                                      [Set (Var "x", Var "y");
                                       LambdaSimple ([], Set (Var "x", Const (Sexpr (Number (Int 1)))))]),
                      [Const (Sexpr (Symbol "a"))]),
                   LambdaSimple (["x"], Set (Var "x", Var "y")))
             )
          )
      );
      "last_year_6">::(fun _ ->
        assert_equal_expr_tag
          (
            LambdaOpt' (["x";"y";"z"], "w", Seq' ([Var' (VarParam ("z", 2));ApplicTP' (LambdaSimple' ([], Seq' ([Set' (Var' (VarBound ("w", 0, 3)), Var' (VarBound ("w", 0, 3)));ApplicTP' (Applic' (Var' (VarBound ("y", 0, 1)), [Var' (VarBound ("x", 0, 0))]), [])])), [])]))
          )
          (Semantics.run_semantics(
               LambdaOpt (["x"; "y"; "z"], "w",
                          Seq
                            [Var "z";
                             Applic
                               (LambdaSimple ([],
                                              Seq [Set (Var "w", Var "w"); Applic (Applic (Var "y", [Var "x"]), [])]),
                                [])])
             )
          )
      );
      "last_year_7">::(fun _ ->
        assert_equal_expr_tag
          (
            Def' (Var' (VarFree "a"), Applic' (LambdaSimple' ([], LambdaOpt' ([], "x", Seq' ([Var' (VarParam ("x", 0));LambdaOpt' ([], "y", Set' (Var' (VarParam ("y", 0)), Const' (Sexpr (Number (Int (1))))))]))), []))
          )
          (Semantics.run_semantics(
               Def (Var "a",
                    Applic
                      (LambdaSimple ([],
                                     LambdaOpt ([], "x",
                                                Seq
                                                  [Var "x";
                                                   LambdaOpt ([], "y", Set (Var "y", Const (Sexpr (Number (Int 1)))))])),
                       []))
             )
          )
      );
      "last_year_8">::(fun _ ->
        assert_equal_expr_tag
          (
            LambdaSimple' (["x";"y"],
                           Seq' ([
                                 Set' (Var' (VarParam ("x", 0)), Box' (VarParam ("x", 0)));
                                 Set' (Var' (VarParam ("y", 1)), Box' (VarParam ("y", 1)));
                                 Seq' ([
                                       Applic' (BoxGet' (VarParam ("x", 0)), [BoxGet' (VarParam ("y", 1))]);
                                       LambdaSimple' (
                                           [],
                                           LambdaSimple' ([],
                                                          LambdaSimple' ([],
                                                                         BoxSet' (VarBound ("x", 2, 0),
                                                                                  Applic' (
                                                                                      LambdaSimple' (["z"],
                                                                                                     BoxSet' (VarBound ("y", 3, 1), BoxGet' (VarBound ("x", 3, 0)))
                                                                                        ),
                                                                                      [BoxGet' (VarBound ("y", 2, 1))]
                                                                                    )
                                                                           )
                                                            )
                                             )
                                         )
                                     ]
                                   )
                               ]
                             )
              )
          
          )
          (Semantics.run_semantics(
               LambdaSimple (["x"; "y"],
                             Seq
                               [Applic (Var "x", [Var "y"]);
                                LambdaSimple ([],
                                              LambdaSimple ([],
                                                            LambdaSimple ([],
                                                                          Set (Var "x",
                                                                               Applic (LambdaSimple (["z"], Set (Var "y", Var "x")), [Var "y"])))))])
             )
          )
      );
      "last_year_9">::(fun _ ->
        assert_equal_expr_tag
          (
            LambdaSimple' ([], Seq' ([Applic' (LambdaSimple' ([], Var' (VarFree "x")), []);Applic' (LambdaSimple' (["x"], Seq' ([Set' (Var' (VarParam ("x", 0)), Box' (VarParam ("x", 0)));Seq' ([BoxSet' (VarParam ("x", 0), Const' (Sexpr (Number (Int (1)))));LambdaSimple' ([], BoxGet' (VarBound ("x", 0, 0)))])])), [Const' (Sexpr (Number (Int (2))))]);ApplicTP' (LambdaOpt' ([], "x", Var' (VarParam ("x", 0))), [Const' (Sexpr (Number (Int (3))))])]))
          )
          (Semantics.run_semantics(
               LambdaSimple ([],
                             Seq
                               [Applic (LambdaSimple ([], Var "x"), []);
                                Applic
                                  (LambdaSimple (["x"],
                                                 Seq
                                                   [Set (Var "x", Const (Sexpr (Number (Int 1))));
                                                    LambdaSimple ([], Var "x")]),
                                   [Const (Sexpr (Number (Int 2)))]);
                                Applic (LambdaOpt ([], "x", Var "x"), [Const (Sexpr (Number (Int 3)))])])
             )
          )
      );
    ]
;;


let single =
  "single">:::
    [
      "last_year_9">::(fun _ ->
        assert_equal_expr_tag
          (
            LambdaSimple' ([], Seq' ([Applic' (LambdaSimple' ([], Var' (VarFree "x")), []);Applic' (LambdaSimple' (["x"], Seq' ([Set' (Var' (VarParam ("x", 0)), Box' (VarParam ("x", 0)));Seq' ([BoxSet' (VarParam ("x", 0), Const' (Sexpr (Number (Int (1)))));LambdaSimple' ([], BoxGet' (VarBound ("x", 0, 0)))])])), [Const' (Sexpr (Number (Int (2))))]);ApplicTP' (LambdaOpt' ([], "x", Var' (VarParam ("x", 0))), [Const' (Sexpr (Number (Int (3))))])]))
          )
          (Semantics.run_semantics(
               LambdaSimple ([],
                             Seq
                               [Applic (LambdaSimple ([], Var "x"), []);
                                Applic
                                  (LambdaSimple (["x"],
                                                 Seq
                                                   [Set (Var "x", Const (Sexpr (Number (Int 1))));
                                                    LambdaSimple ([], Var "x")]),
                                   [Const (Sexpr (Number (Int 2)))]);
                                Applic (LambdaOpt ([], "x", Var "x"), [Const (Sexpr (Number (Int 3)))])])
             )
          )
      );
    ]
;;    
  

let () =
  run_test_tt_main simple_suite;
  run_test_tt_main last_year;
  run_test_tt_main boxing;
  (* run_test_tt_main single; *)
;;
