#use "semantic-analyser.ml";;

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
  let rec get_consts = function
    |Const'(c) ->[c]
    |Var'(_) -> []
    |Box'(_) -> []
    |BoxGet'(_) -> []
    |BoxSet'(_, e) -> (get_consts e)
    |If'(test,dit,dif) -> (get_consts test)^(get_consts dit)^(get_consts dif)
    (* | Seq'
     * | Set' of expr' * expr'
     * | Def' of expr' * expr'
     * | Or' of expr' list
     * | LambdaSimple' of string list * expr'
     * | LambdaOpt' of string list * string * expr'
     * | Applic' of expr' * (expr' list)
     * | ApplicTP' of expr' * (expr' list) *)
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
      let (sexpr, (index, _)) = get_last accin
      let sob_size = (calc_sob_size sexpr) in
      let next_index = index + sob_size in
      let byte_rep = gen_const_byte_rep curr in
      acc::(curr, (next_index, byte_rep)) in
    List.fold_left fold_func firsts rest;;

  ;;
  let make_fvars_tbl asts = [];;
  let generate consts fvars e = "mov rax, const_tbl+6*1";;
end;;

