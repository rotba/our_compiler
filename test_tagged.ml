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
  |TaggedSexpr(s,e) -> String.concat "" ["TaggedSexpr( "; s; " , "; (sexpr_to_string e) ;" )"]
  |TagRef(x)->x
  |_ -> "not_implemented";;

let rec expr_to_string  =
  function
  |Const(Sexpr(s)) -> String.concat "" ["Const ( Sexpr ( "; (sexpr_to_string s); " ) )"]
  |_->"not_implemented";;

let assert_equal_expr expr1 expr2=
  assert_equal
    ~cmp: expr_eq
    ~printer:expr_to_string
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
        "(lambda (x . y) x)">::(fun _ ->
          assert_equal_expr
            (LambdaOpt(["x"],"y" ,Var("x")))
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
      ];;




let () =
  run_test_tt_main simple_suite;
  run_test_tt_main less_simple_suite;
;;
