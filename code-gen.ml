#use "semantic-analyser.ml";;
exception X_not_yet_implemented of string;;
exception X_bug_error_m of string;;
let raise_not_imp func_name no_match to_string =
  let msg =func_name ^": " in
  let msg = msg^(to_string no_match) in
  raise (X_not_yet_implemented msg);
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
  let consts_table_address = "const_tbl";;
  let const_address idx = consts_table_address^"+"^(string_of_int idx);;
  
  let rec extend_constants const=
    let rec sextend_constants = function
      |(Number(_)|Nil|Bool _|Char _|String _) as id -> [Sexpr(id)]
      |Pair(car,cdr) as id ->
        let ex_car = (sextend_constants car) in
        let ex_cdr = (sextend_constants car) in
        [Sexpr(id)]@ex_car@ex_cdr
      |no_match -> raise_not_imp "sextend_constants" (Const(Sexpr(no_match))) exp_to_string
    in
    match const with
    |Void ->[Void]
    |Sexpr(s) -> sextend_constants s
  let rec get_last = function
    |[last] -> last
    |car::cdr -> get_last cdr
    |_ -> raise (X_bug_error_m "get last");;
  let rec calc_const_sob_size = function
    |Void -> type_size
    |(Sexpr(Number(Int(_))) |Sexpr(Number(Float(_))))->type_size + 8
    |Sexpr(Bool(_))->type_size + 1
    |Sexpr(Pair(_,_)) -> type_size + 2*8
    |no_match ->
      let msg = "calc_const_sob_size :" in
      let msg = msg^(exp_to_string (Const(no_match))) in
      raise (X_not_yet_implemented msg);;
  ;;
  
  let rec get_consts= function
    |Const'(c) ->[c]
    |_ ->raise (X_not_yet_implemented "get_consts");;
  ;;

  let equal_consts c1 c2 =
    match c1,c2 with
    |Sexpr(s1),Sexpr(s2) -> sexpr_eq s1 s2
    |Void,Void -> true
    |_ ->false;;

  let rec remove_duplicates = function
    |[] -> []
    |car::cdr->
      let exists = List.exists (equal_consts car) cdr in
      if(exists)
      then
        (remove_duplicates cdr)
      else
        car::(remove_duplicates cdr) ;;


  let rec find_rel_idx const rest =
      match rest with
      |[] -> raise (X_bug_error_m "find_rel_idx")
      |(car,(index,_))::cdr ->
        if((equal_consts car const))
        then
          index
        else
          (find_rel_idx const cdr);;
  
   let gen_const_byte_rep curr rest=
    match curr with
    |Sexpr(Number(Int(vall))) -> "MAKE_LITERAL_INT("^(string_of_int vall) ^")"
    |Sexpr(Pair(car,cdr)) ->
      let car_rel_idx = (find_rel_idx (Sexpr(car)) rest) in
      let cdr_rel_idx = (find_rel_idx (Sexpr(cdr)) rest)  in
      let car_address = const_address car_rel_idx in
      let cdr_address = const_address cdr_rel_idx in
      "MAKE_LITERAL_PAIR("^car_address^","^cdr_address^")"
    |_ -> raise (X_not_yet_implemented "get_const_byte_rep");;
   
  let make_consts_tbl asts =
    let firsts =  
    [
      (Void,(0, "MAKE_VOID"));
      (Sexpr(Nil),(1, "MAKE_NIL"));
      (Sexpr(Bool(false)) ,(2 , "MAKE_BOOL(0)"));
      (Sexpr(Bool(true)) ,(4 , "MAKE_BOOL(1)"));
    ]
    in
    let fold_func curr acc = (get_consts curr)@acc in
    let rest = List.fold_right fold_func asts [] in
    let rest = List.map extend_constants rest in
    let fold_func acc curr = acc@curr in
    let rest = List.fold_left fold_func [] rest in
    let rest = remove_duplicates rest in
    let fold_func curr acc =
      let ((sexpr, (index, _))::_) = acc in
      let sob_size = (calc_const_sob_size sexpr) in
      let next_index = index + sob_size in
      let byte_rep = gen_const_byte_rep curr acc in
      List.append [(curr, (next_index, byte_rep))] acc in
    List.fold_right fold_func rest firsts;;

  ;;
  let make_fvars_tbl asts = [];;

  
  let generate consts fvars e =
    match e with
    |Const'(e) ->
      let address_in_consts = find_rel_idx e consts in
      let address = const_address address_in_consts in 
      let code = Printf.sprintf "mov rax, %s" address in
      code
    |no_match -> raise_not_imp "generate" no_match exp'_to_string
  ;;
  
end;;

