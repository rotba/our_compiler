#use "semantic-analyser.ml";;
exception X_not_yet_implemented of string;;
(* This module is here for you convenience only!
   You are not required to use it.
   you are allowed to change it. *)
module type CODE_GEN = sig
  (* This signature assumes the structure of the constants table is
     a list of key-value pairs:
     - The keys are constant values (Sexpr(x) or Void)
     - The values are pairs of:
       * the offset from the base const_table address in bytes; and
       * a string containing the byte representation (or a sequence of nasm macros)
         of the constant value
     For example: [(Sexpr(Nil), (1, "SOB_NIL"))]
   *)
  val make_consts_tbl : expr' list -> (constant * (int * string)) list

  (* This signature assumes the structure of the fvars table is
     a list of key-value pairs:
     - The keys are the fvar names as strings
     - The values are the offsets from the base fvars_table address in bytes
     For example: [("boolean?", 0)]
   *)  
  val make_fvars_tbl : expr' list -> (string * int) list

  (* This signature represents the idea of outputing assembly code as a string
     for a single AST', given the full constants and fvars tables. 
   *)
  val generate : (constant * (int * string)) list -> (string * int) list -> expr' -> string
end;;

module Code_Gen : CODE_GEN = struct
  let type_size =1;;
  let rec get_last = function
    |[last] -> last
    |car::cdr -> get_last cdr
    |_ -> raise X_bug_error;;
  let calc_const_sob_size = function
    |(Sexpr(Number(Int(_))) |Sexpr(Number(Float(_))))->type_size + 8
    |Sexpr(Bool(_))->type_size + 1
    |no_match ->
      let msg = "calc_const_sob_size :" in
      let msg = msg^(exp_to_string (Const(no_match))) in
      raise (X_not_yet_implemented msg);;
  ;;

  let gen_const_byte_rep =function
    |Sexpr(Number(Int(vall))) -> "MAKE_LITERAL_INT("^(string_of_int vall) ^")"
    |_ -> raise (X_not_yet_implemented "get_const_byte_rep");;
  
  let rec get_consts= function
    |Const'(c) ->[c]
    |_ ->raise (X_not_yet_implemented "get_consts");;
  ;;
                
  let make_consts_tbl asts =
    let firsts = 
    [
      (Void,(0, "MAKE_VOID"));
      (Sexpr(Nil),(1, "MAKE_NIL"));
      (Sexpr(Bool(false)) ,(2 , "MAKE_BOOL(0)"));
      (Sexpr(Bool(true)) ,(4 , "MAKE_BOOL(1)"));
    ]
    in
    let fold_func acc curr = List.concat [acc; (get_consts curr)] in
    let rest = List.fold_left fold_func [] asts in
    let fold_func acc curr =
      let (sexpr, (index, _)) = get_last acc in
      let sob_size = (calc_const_sob_size sexpr) in
      let next_index = index + sob_size in
      let byte_rep = gen_const_byte_rep curr in
      List.append acc [(curr, (next_index, byte_rep))] in
    List.fold_left fold_func firsts rest;;

  ;;
  let make_fvars_tbl asts = [];;
  let generate consts fvars e = "mov rax, const_tbl+6*1";;
end;;

