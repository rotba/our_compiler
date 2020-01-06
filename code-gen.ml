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
  let make_consts_tbl asts =
    [
      (Void,(0, "MAKE_VOID"));
      (Sexpr(Nil),(1, "MAKE_NIL"));
      (Sexpr(Bool(false)) ,(2 , "MAKE_BOOL(0)"));
      (Sexpr(Bool(true)) ,(4 , "MAKE_BOOL(1)"));
      (Sexpr(Number(Int(1))), (6,"MAKE_LITERAL_INT(1)" ))
    ]
  ;;
  let make_fvars_tbl asts = [];;
  let generate consts fvars e = "mov rax, qword[const_tbl+6*1]";;
end;;

