#use "tag-parser.ml";;
open Tag_Parser;;


exception X_wrong;;


let test exp res num= if ( tag_parse_expressions exp =res) 
then (print_string (Printf.sprintf "Test %d passed\n" num))
else (print_string (Printf.sprintf "Test %d failed\n" num));;

let input16=[Pair (Symbol "define", Pair (Pair (Symbol "x", Symbol "z"), Pair (Pair (Symbol "eq?", Pair (Char 'y', Pair (Pair (Symbol "car", Pair (Symbol "z", Nil)), Nil))), Nil)))]
  ;;

let output16=[Def (Var "x", LambdaOpt ([], "z", Applic (Var "eq?", [Const (Sexpr (Char 'y'));Applic (Var "car", [Var "z"])])))]
  ;;

let input17=[Pair (Symbol "define", Pair (Pair (Symbol "x", Symbol "y"), Pair (Pair (Symbol "eq?", Pair (Pair (Symbol "quote", Pair (Nil, Nil)), Pair (Symbol "y", Nil))), Nil)))];;
let output17=[Def (Var "x", LambdaOpt ([], "y", Applic (Var "eq?", [Const (Sexpr (Nil));Var "y"])))];;

let input18=[Pair (Symbol "letrec", Pair (Pair (Pair (Symbol "loop", Pair (Pair (Symbol "lambda", Pair (Pair (Symbol "x", Nil), Pair (Pair (Symbol "if", Pair (Symbol "x", Pair (Pair (Symbol "set!", Pair (Symbol "y", Pair (Bool true, Nil))), Pair (Pair (Symbol "loop", Pair (Symbol "y", Nil)), Nil)))), Nil))), Nil)), Nil), Pair (Pair (Symbol "loop", Pair (Symbol "x", Nil)), Nil)))];;
let output18=[Applic (LambdaSimple (["loop"], Seq ([Set (Var "loop", LambdaSimple (["x"], If (Var "x", Set (Var "y", Const (Sexpr (Bool true))), Applic (Var "loop", [Var "y"]))));Applic (Var "loop", [Var "x"])])), [Const (Sexpr (Symbol "whatever"))])];;

let input19=[Pair (Symbol "cond", Pair (Pair (Bool true, Pair (Number (Int (1)), Pair (Number (Int (2)), Pair (Number (Int (3)), Nil)))), Nil))];;
let output19=[If (Const (Sexpr (Bool true)), Seq ([Const (Sexpr (Number (Int (1))));Const (Sexpr (Number (Int (2))));Const (Sexpr (Number (Int (3))))]), Const (Void))];;

let input20=[Pair (Symbol "let*", Pair (Pair (Pair (Symbol "x", Pair (Number (Int (1)), Nil)), Pair (Pair (Symbol "y", Pair (Pair (Symbol "lambda", Pair (Nil, Pair (Symbol "x", Nil))), Nil)), Nil)), Pair (Symbol "x", Pair (Pair (Symbol "y", Nil), Nil))))];;
let output20=[Applic (LambdaSimple (["x"], Applic (LambdaSimple (["y"], Seq [Var "x"; Applic (Var "y", [])]), [LambdaSimple ([], Var "x")])), [Const (Sexpr (Number (Int (1))))])];;

let input21=[Pair (Symbol "if", Pair (Pair (Symbol "if", Pair (Symbol "a", Pair (Symbol "b", Pair (Char 'c', Nil)))), Pair (Pair (Symbol "quote", Pair (Symbol "x", Nil)), Pair (Pair (Symbol "quote", Pair (Pair (Symbol "+", Pair (Symbol "y", Pair (Symbol "z", Nil))), Nil)), Nil))))];;
let output21=[If (If (Var "a", Var "b", Const (Sexpr (Char 'c'))), Const (Sexpr (Symbol "x")), Const (Sexpr (Pair (Symbol "+", Pair (Symbol "y", Pair (Symbol "z", Nil))))))];;

let input22=[Pair (Symbol "or", Pair (Pair (Symbol "zero?", Pair (Symbol "x", Nil)), Pair (Pair (Symbol "zero?", Pair (Symbol "y", Nil)), Pair (Pair (Symbol "zero?", Pair (Symbol "z", Nil)), Pair (Pair (Symbol "zero?", Pair (Symbol "w", Nil)), Pair (Pair (Symbol "zero?", Pair (Symbol "v", Nil)), Nil))))))];;
let output22=[Or ([Applic (Var "zero?", [Var "x"]);Applic (Var "zero?", [Var "y"]);Applic (Var "zero?", [Var "z"]);Applic (Var "zero?", [Var "w"]);Applic (Var "zero?", [Var "v"])])];;

let input23= [Pair (Symbol "and", Pair (Pair (Symbol "zero?", Pair (Symbol "x", Nil)), Pair (Pair (Symbol "zero?", Pair (Symbol "y", Nil)), Pair (Pair (Symbol "zero?", Pair (Symbol "z", Nil)), Pair (Pair (Symbol "zero?", Pair (Symbol "w", Nil)), Pair (Pair (Symbol "zero?", Pair (Symbol "v", Nil)), Nil))))))];;
let output23=[If (Applic (Var "zero?", [Var "x"]), If (Applic (Var "zero?", [Var "y"]), If (Applic (Var "zero?", [Var "z"]), If (Applic (Var "zero?", [Var "w"]), Applic (Var "zero?", [Var "v"]), Const (Sexpr (Bool false))), Const (Sexpr (Bool false))), Const (Sexpr (Bool false))), Const (Sexpr (Bool false)))];;

let input24=[Pair (Symbol "if", Pair (Pair (Symbol "begin", Pair (Number (Int (1)), Pair (Number (Int (2)), Pair (Number (Int (3)), Pair (Bool false, Nil))))), Pair (Number (Int (1)), Pair (Pair (Symbol "lambda", Pair (Nil, Pair (Pair (Symbol "begin", Nil), Pair (Number (Int (1)), Pair (Number (Int (2)), Pair (Number (Int (3)), Pair (Bool false, Nil))))))), Nil))))];;
let output24=[If (Seq ([Const (Sexpr (Number (Int (1))));Const (Sexpr (Number (Int (2))));Const (Sexpr (Number (Int (3))));Const (Sexpr (Bool false))]), Const (Sexpr (Number (Int (1)))), LambdaSimple ([], Seq ([Const (Void);Const (Sexpr (Number (Int (1))));Const (Sexpr (Number (Int (2))));Const (Sexpr (Number (Int (3))));Const (Sexpr (Bool false))])))];;

let input26=[Pair (Symbol "define", Pair (Pair (Symbol "foo", Pair (Symbol "x", Pair (Symbol "y", Nil))), Pair (Pair (Symbol "lambda", Pair (Symbol "y", Pair (Symbol "y", Pair (Symbol "y", Nil)))), Pair (Pair (Symbol "lambda", Pair (Pair (Symbol "z", Symbol "w"), Pair (Symbol "z", Pair (Symbol "w", Nil)))), Pair (Pair (Symbol "lambda", Pair (Pair (Symbol "z", Nil), Pair (Pair (Symbol "lambda", Pair (Symbol "w", Pair (Symbol "w", Nil))), Nil))), Nil)))))];;
let output26=[Def (Var "foo", LambdaSimple (["x";"y"], Seq ([LambdaOpt ([], "y", Seq ([Var "y";Var "y"]));LambdaOpt (["z"], "w", Seq ([Var "z";Var "w"]));LambdaSimple (["z"], LambdaOpt ([], "w", Var "w"))])))];;

let input27=[Pair (Symbol "quasiquote", Pair (Pair (Pair (Symbol "unquote", Pair (Symbol "a", Nil)), Pair (Symbol "b", Pair (Pair (Symbol "unquote-splicing", Pair (Symbol "c", Nil)), Nil))), Nil))];;
let output27=[Applic (Var "cons", [Var "a";Applic (Var "cons", [Const (Sexpr (Symbol "b"));Applic (Var "append", [Var "c";Const (Sexpr (Nil))])])])];;

let input28=[Pair (Pair (Symbol "quasiquote", Pair (Pair (Pair (Symbol "unquote-splicing", Pair (Symbol "x", Nil)), Pair (Pair (Symbol "unquote", Pair (Pair (Symbol "set!", Pair (Symbol "y", Pair (Pair (Symbol "car", Pair (Symbol "x", Nil)), Nil))), Nil)), Nil)), Nil)), Nil)];;
let output28=[Applic (Applic (Var "append", [Var "x";Applic (Var "cons", [Set (Var "y", Applic (Var "car", [Var "x"]));Const (Sexpr (Nil))])]), [])];;

let input29=[Pair (Symbol "let", Pair (Pair (Pair (Symbol "x", Pair (Pair (Symbol "let", Pair (Pair (Pair (Symbol "x", Pair (Number (Int (1)), Nil)), Nil), Pair (Pair (Symbol "let", Pair (Pair (Pair (Symbol "x", Pair (Number (Int (2)), Nil)), Nil), Pair (Symbol "x", Nil))), Nil))), Nil)), Nil), Pair (Pair (Symbol "set!", Pair (Symbol "x", Pair (Pair (Symbol "quote", Pair (Symbol "let", Nil)), Nil))), Nil)))];;
let output29=[Applic (LambdaSimple (["x"], Set (Var "x", Const (Sexpr (Symbol "let")))), [Applic (LambdaSimple (["x"], Applic (LambdaSimple (["x"], Var "x"), [Const (Sexpr (Number (Int (2))))])), [Const (Sexpr (Number (Int (1))))])])];;

let input30=[Pair (Symbol "cond", Pair (Pair (Symbol "x", Pair (Pair (Symbol "begin", Pair (Symbol "x", Nil)), Nil)), Pair (Pair (Pair (Symbol "lambda", Pair (Pair (Symbol "x", Nil), Pair (Symbol "x", Nil))), Pair (Symbol "=>", Pair (Pair (Symbol "cond", Pair (Pair (Symbol "else", Pair (Pair (Symbol "lambda", Pair (Pair (Symbol "x", Nil), Pair (Pair (Symbol "x", Nil), Nil))), Nil)), Nil)), Nil))), Pair (Pair (Symbol "else", Pair (Symbol "value", Nil)), Nil))))];;
let output30=[If (Var "x", Var "x", Applic (LambdaSimple (["value";"f";"rest"], If (Var "value", Applic (Applic (Var "f", []), [Var "value"]), Applic (Var "rest", []))), [LambdaSimple (["x"], Var "x");LambdaSimple ([], LambdaSimple (["x"], Applic (Var "x", [])));LambdaSimple ([], Var "value")]))];;

let input31=[Pair (Symbol "or", Pair (Pair (Symbol "begin", Nil), Pair (Pair (Symbol "and", Nil), Nil)))];;
let output31=[Or ([Const (Void);Const (Sexpr (Bool true))])];;

let input32=[Pair (Symbol "+", Pair (Pair (Symbol "and", Pair (Number (Int (1)), Nil)), Pair (Pair (Symbol "or", Pair (Number (Int (2)), Nil)), Nil)))];;
let output32=[Applic (Var "+", [Const (Sexpr (Number (Int (1))));Const (Sexpr (Number (Int (2))))])];;

let input33=[Pair (Symbol "quote", Pair (Pair (Symbol "cond", Pair (Pair (Symbol "let", Nil), Pair (Pair (Symbol "or", Pair (Symbol "and", Nil)), Nil))), Nil))];;
let output33=[Const (Sexpr (Pair (Symbol "cond", Pair (Pair (Symbol "let", Nil), Pair (Pair (Symbol "or", Pair (Symbol "and", Nil)), Nil)))))];;

let input34=[Pair (Symbol "begin", Pair (Pair (Symbol "lambda", Pair (Nil, Pair (Pair (Symbol "begin", Pair (Pair (Symbol "quote", Pair (Symbol "a", Nil)), Pair (Pair (Symbol "quote", Pair (Symbol "b", Nil)), Nil))), Pair (Pair (Symbol "quote", Pair (Symbol "c", Nil)), Nil)))), Pair (Pair (Symbol "begin", Pair (Pair (Symbol "quote", Pair (Symbol "d", Nil)), Nil)), Nil)))];;
let output34=[Seq ([LambdaSimple ([], Seq ([Seq ([Const (Sexpr (Symbol "a"));Const (Sexpr (Symbol "b"))]);Const (Sexpr (Symbol "c"))]));Const (Sexpr (Symbol "d"))])];;

let input35=[Pair (Pair (Symbol "lambda", Pair (Pair (Symbol "c", Nil), Pair (Pair (Pair (Symbol "lambda", Pair (Pair (Symbol "a", Nil), Pair (Symbol "a", Pair (Pair (Symbol "quote", Pair (Symbol "b", Nil)), Nil)))), Pair (Symbol "c", Nil)), Nil))), Pair (Pair (Symbol "begin", Pair (Pair (Symbol "quote", Pair (Symbol "d", Nil)), Nil)), Nil))];;
let output35=[Applic (LambdaSimple (["c"], Applic (LambdaSimple (["a"], Seq ([Var "a";Const (Sexpr (Symbol "b"))])), [Var "c"])), [Const (Sexpr (Symbol "d"))])];;

let input36=[Number (Int 1); Number (Int 2); Symbol "x"; Symbol "y"; Symbol "z"];;
let output36=[Const (Sexpr (Number (Int (1))));Const (Sexpr (Number (Int (2))));Var "x";Var "y";Var "z"];;

let input37=[Pair (Symbol "define",
Pair (Pair (Symbol "foo", Pair (Symbol "x", Nil)),
 Pair (Pair (Symbol "zero?", Pair (Symbol "x", Nil)), Nil)));
Pair (Symbol "cond",
Pair
 (Pair (Bool false, Pair (Symbol "=>", Pair (Symbol "foo", Nil))),
 Nil))];;

let output37=[Def (Var "foo", LambdaSimple (["x"], Applic (Var "zero?", [Var "x"])));Applic (LambdaSimple (["value";"f"], If (Var "value", Applic (Applic (Var "f", []), [Var "value"]), Const (Void))), [Const (Sexpr (Bool false));LambdaSimple ([], Var "foo")])];;



let run16 =test input16 output16 16;;
let run17=test input17 output17 17;;
let run18=test input18 output18 18;;
let run19=test input19 output19 19;;
let run20=test input20 output20 20;;
let run21=test input21 output21 21;;
let run22=test input22 output22 22;;
let run23=test input23 output23 23;;
let run24=test input24 output24 24;;
let run26=test input26 output26 26;;
let run27=test input27 output27 27;;
let run28=test input28 output28 28;;
let run29=test input29 output29 29;;
let run30=test input30 output30 30;;
let run31=test input31 output31 31;;
let run32=test input32 output32 32;;
let run33=test input33 output33 33;;
let run34=test input34 output34 34;;
let run35=test input35 output35 35;;
let run36=test input36 output36 36;;
let run37=test input37 output37 37;;

let test_list=[run16;run17;run18;run19;run20;run21;run22;run23;run24;run26;run27;run28;run29;run30;run31;run32;run33;run34;run35;run36;run37];;

let iden test_run=(test_run);;

let test = List.map iden test_list;;