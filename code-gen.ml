#use "semantic-analyser.ml";;

exception X_not_yet_implemented of string;;
exception X_bug_error_m of string;;
let elements_on_stack = 4;;
let elements_on_stack_no_rbp = 3;;
let raise_not_imp func_name no_match to_string =
  let msg =func_name ^": " in
  let msg = msg^(to_string no_match) in
  raise (X_not_yet_implemented msg);



module Labels : sig
  val make_label : string -> string
  val make_labels : string list -> string list
end = struct
  let label_counter = ref 0;;
    
  let count () =
    ( label_counter := 1 + !label_counter ;
      !label_counter );;
    
  let make_label base =
    Printf.sprintf "%s_%d" base (count());;
    
  let make_labels bases =
    let n = count() in
    List.map (fun base -> Printf.sprintf "%s_%d" base n)
        bases;;
end;;

(* This module is here for you convenience only!
   You are not required to use it.
   you are allowed to change it. *)
module Labels : sig
  val make_label : string -> string
  val make_labels : string list -> string list
end = struct
  let label_counter = ref 0;;
    
  let count () =
    ( label_counter := 1 + !label_counter ;
      !label_counter );;
    
  let make_label base =
    Printf.sprintf "%s_%d" base (count());;
    
  let make_labels bases =
    let n = count() in
    List.map (fun base -> Printf.sprintf "%s_%d" base n)
	     bases;;
end;;module Labels : sig
  val make_label : string -> string
  val make_labels : string list -> string list
end = struct
  let label_counter = ref 0;;
    
  let count () =
    ( label_counter := 1 + !label_counter ;
      !label_counter );;
    
  let make_label base =
    Printf.sprintf "%s_%d" base (count());;
    
  let make_labels bases =
    let n = count() in
    List.map (fun base -> Printf.sprintf "%s_%d" base n)
	     bases;;
end;;
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

  let rec sextend_constants = function
    |(Number(_)|Nil|Bool _|Char _|String _) as id -> [Sexpr(id)]
    |Symbol(s) as id -> [Sexpr(id);Sexpr(String(s))]
    |Pair(car,cdr) as id ->
      let ex_car = (sextend_constants car) in
      let ex_cdr = (sextend_constants cdr) in
      [Sexpr(id)]@ex_car@ex_cdr
    |no_match -> raise_not_imp "sextend_constants" (Const(Sexpr(no_match))) exp_to_string
  ;;
  let rec extend_constants const=
    match const with
    |Void ->[Void]
    |Sexpr(s) -> sextend_constants s
  let rec get_last = function
    |[last] -> last
    |car::cdr -> get_last cdr
    |_ -> raise (X_bug_error_m "get last");;
  
  let rec calc_const_sob_size = function
    |Void -> type_size
    |Sexpr(Nil) -> type_size
    |(Sexpr(Number(Int(_))) |Sexpr(Number(Float(_))))->type_size + 8
    |Sexpr(String(s))->type_size + 8 + (String.length s)
    |Sexpr(Bool(_))->type_size + 1
    |Sexpr(Pair(_,_)) -> type_size + 2*8
    |Sexpr(Symbol(_)) -> type_size +8
    |no_match ->
      let msg = "calc_const_sob_size :" in
      let msg = msg^(exp_to_string (Const(no_match))) in
      raise (X_not_yet_implemented msg);;
  ;;
  
  let rec get_consts= function
    | Const'(c) ->[c]
    | Var'(_) ->[]
    | Box'(_) -> []
    | BoxGet'(_) -> []
    | BoxSet'(_,e) -> get_consts e
    | If'(a,b,c) -> (get_consts a)@(get_consts b)@(get_consts c)
    | Seq'(lst) -> (List.fold_left (fun a b -> a@(get_consts b)) [] lst)
    | Set'(a,b) -> (get_consts a)@(get_consts b)
    | Def'(a,b) -> (get_consts a)@(get_consts b)
    | Or'(lst) -> (List.fold_left (fun a b -> a@(get_consts b)) [] lst)
    | LambdaSimple'(_,e) -> (get_consts e)
    | LambdaOpt'(_,_,e) -> (get_consts e)
    | Applic'(e,es) -> (get_consts e)@(List.fold_left (fun a b -> a@(get_consts b)) [] es)
    | ApplicTP'(e,es) -> (get_consts e)@(List.fold_left (fun a b -> a@(get_consts b)) [] es);;
    (* |_ ->raise (X_not_yet_implemented "get_consts");; *)
  

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
      |[] -> raise (X_bug_error_m (Printf.sprintf "find_rel_idx: %s"  (exp_to_string (Const(const)))))
      |(car,(index,_))::cdr ->
        if((equal_consts car const))
        then
          index
        else
          (find_rel_idx const cdr);;
  
   let gen_const_byte_rep curr rest=
     match curr with
     |Void -> "MAKE_VOID"
     |Sexpr(Nil) -> "MAKE_NIL"
     |Sexpr(Bool(false)) -> "MAKE_BOOL(0)"
     |Sexpr(Bool(true)) -> "MAKE_BOOL(1)"
     |Sexpr(Number(Int(vall))) -> Printf.sprintf "MAKE_LITERAL_INT(%s)" (string_of_int vall)
     |Sexpr(String(s)) -> Printf.sprintf "MAKE_LITERAL_STRING \"%s\", %d" s (String.length s)
     |Sexpr(Symbol(s)) ->
       let sym_str_rel_idx = (find_rel_idx (Sexpr(String(s))) rest) in
       let sym_str_address = const_address sym_str_rel_idx in
       Printf.sprintf "MAKE_LITERAL_SYMBOl(%s)" sym_str_address
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
      Void;
      Sexpr(Nil);
      Sexpr(Bool(false));
      Sexpr(Bool(true));
    ]
    in
    let fold_func curr acc = (get_consts curr)@acc in
    let consts = List.fold_right fold_func asts [] in
    let consts = List.map extend_constants consts in
    let fold_func acc curr = acc@curr in
    let consts = List.fold_left fold_func [] consts in
    let consts = consts@firsts in
    let consts = remove_duplicates consts in
    let fold_func curr acc =
      let index =
        match acc with
        |[] -> 0
        |acc ->
          let (sexpr, (index, _)) = get_last acc in
          let sob_size = (calc_const_sob_size sexpr) in
          index +sob_size
      in
      let byte_rep = gen_const_byte_rep curr acc in
      List.append acc [(curr, (index, byte_rep))]  in
    List.fold_right fold_func consts [];;

  ;;
  let rec get_fvars =
    let fold a b = a@(get_fvars b) in
    function
    | Var'(VarFree(v)) -> [v]
    | Box'(VarFree(v)) -> [v]
    | BoxGet'(VarFree(v)) -> [v]
    | BoxSet'(VarFree(v), b) -> [v]@(get_fvars b)
    | If'(a,b,c) -> (get_fvars a)@(get_fvars b)@(get_fvars c)
    | Seq'(lst) -> (List.fold_left fold [] lst)
    | Set'(a,b) -> (get_fvars a)@(get_fvars b)
    | Def'(a,b) -> (get_fvars a)@(get_fvars b)
    | Or'(lst) -> (List.fold_left fold [] lst)
    | LambdaSimple'(sl, e) -> (get_fvars e)
    | LambdaOpt'(sl,s, e) -> (get_fvars e)
    | Applic'(e,lst) -> (get_fvars e)@(List.fold_left fold [] lst)
    | ApplicTP'(e,lst) -> (get_fvars e)@(List.fold_left fold [] lst)
    | _ -> [];;

  (* (reset_id());; *)
  let make_fvars_tbl asts = 
    let id = ref (-1) in
  let get_id = 
    let func = fun () ->(
      id := !id + 1; !id
    ) in
    func in
    let cons_uniq xs x = if List.mem x xs then xs else x :: xs in
    let remove_from_left xs = List.rev (List.fold_left cons_uniq [] xs) in
    let fold_fun a b = a@(get_fvars b) in
    let vars = (List.fold_left fold_fun ["boolean?"; "float?"; "integer?"; "pair?";
    "null?"; "char?"; "string?";
    "procedure?"; "symbol?"; "string-length";
    "string-ref"; "string-set!"; "make-string";
    "symbol->string"; 
    "char->integer"; "integer->char"; "eq?";
    "+"; "*"; "-"; "/"; "<"; "=";
    "cons";"car";] asts) in
    let vars = remove_from_left vars in 
    let fold_fun acc b = (acc@[(b, (get_id()))]) in
    List.fold_left fold_fun [] vars 
    ;;

  let find_fvar_indx v fvars = 
    let pair = (List.find_opt (fun x -> match x with (a,b) -> String.equal a v) fvars) in 
    match pair with
    | Some((a,b)) -> b
    | _ -> raise (X_bug_error_m  "fail find_fvar_indx: :( ");;

  let concat_lines lines =
    String.concat "\n" lines;;

  
    
  
  let rec generate consts fvars e =
    let generate_push_args args =
      let fold_args curr acc=
        let arg_i = (generate consts fvars curr) in
        let push_res = "push rax" in
        (concat_lines 
           [
             acc;
             arg_i;
             push_res
           ]
        )
      in
      List.fold_right fold_args args ""
    in
    let verify_closure = "" in
    match e with
    |Const'(e) ->
      let address_in_consts = find_rel_idx e consts in
      let address = const_address address_in_consts in 
      let code = Printf.sprintf "mov rax, %s" address in
      code
    | Set'(Var'(VarFree(v)),e) -> 
      let code = generate consts fvars e in
      let address_in_vars = find_fvar_indx v fvars in
      let code = code ^ (Printf.sprintf "mov qword[fvar_tbl+%d*8] \n rax mov rax \n sob_void" address_in_vars) in
      code
    |Seq'(lst) ->
      let exps = List.map (generate consts fvars) lst in 
      let code = String.concat "\n" exps in
      code
    |Or'(lst) ->
      let exps = List.map (generate consts fvars) lst in 
      let code = String.concat "\ncmp rax, SOB_FALSE_ADDRESS \n jne Lexit \n" exps in
      code ^ "\nLexit:\n"
    | If'(a,b,c) -> 
      let i = (generate consts fvars a) in
      let t = (generate consts fvars b) in
      let e = (generate consts fvars c) in
      let labels = Labels.make_labels ["Lelse";"Lexit"] in
      Printf.sprintf "%s \n cmp rax, SOB_FALSE_ADDRESS \n je %s \n %s \n jmp %s \n %s: \n %s \n %s:\n" i (List.nth labels 0) t (List.nth labels 1) (List.nth labels 0) e (List.nth labels 1)
    | Applic'(e,l) ->
      let push_args = generate_push_args l in
      let push_n = Printf.sprintf "push %d" (List.length l) in
      let proc = generate consts fvars e in
      let push_env =
        (concat_lines
           [
             "CLOSURE_ENV rbx, rax";
             "push rbx"
           ]
        )
      in
      let call_code =
        (concat_lines
           [
             "CLOSURE_CODE rbx, rax";
             "call rbx"
           ]
        )
      in
      (concat_lines
         [
           push_args;
           push_n;
           proc;
           verify_closure;
           push_env;
           call_code;
           "add rsp, 8*1";
           "pop rbx";
           "shl rbx, 3";
           "add rsp, rbx"
         ]
      )

    | ApplicTP'(e,l) ->
       let push_args = generate_push_args l in
       let push_n = Printf.sprintf "push %d" (List.length l) in
       let proc = generate consts fvars e in
       let push_env =
         (concat_lines
            [
              "CLOSURE_ENV rbx, rax";
              "push rbx"
            ]
         )
       in
       let push_old_ret = "push qword[rbp +8*1]" in
       let jmp_code =
         (concat_lines
            [
              "CLOSURE_CODE rbx, rax";
              "jmp rbx"
            ]
         )
       in
       let fix_stack =
         (concat_lines
            [
              Printf.sprintf "SHIFT_FRAME %d" (1*(elements_on_stack_no_rbp+(List.length l)));
            ]
         )
       in
       (concat_lines
          [
            push_args;
            push_n;
            proc;
            verify_closure;
            push_env;
            push_old_ret;
            fix_stack;
            jmp_code;
            "add rsp, 8*1";
            "pop rbx";
            "shl rbx, 3";
            "add rsp, rbx"
          ]
       )

    | Var'(VarParam(_, minor)) -> 
        Printf.sprintf "mov rax, qword [rbp+8*(4+%d)]" minor
    | Set'(Var'(VarParam(_, minor)),e) ->
        String.concat "\n" [
          (generate consts fvars e);
          (Printf.sprintf "mov qword [rbp+8*(4+%d)], rax" minor);
          "mov rax, SOB_VOID_ADDRESS"
        ]

    | Var'(VarBound(_, major, minor)) -> 
       String.concat "\n" [
          "mov rax, qword[rbp+8*2]";
          Printf.sprintf "mov rax, qword[rax+8*%d]" major;
          Printf.sprintf"mov rax, qword[rax+8*%d]" minor
        ]

    | Set'(Var'(VarBound(_,major,minor)),e) -> 
        String.concat "\n" [
          (generate consts fvars e);
          "mov rbx, qword[rbp+8*2]";
          Printf.sprintf "mov rbx, qword[rbx+8*%d]" major;
          Printf.sprintf"mov qword[rbx+8*%d], rax" minor;
          "mov rax, SOB_VOID_ADDRESS"
        ]
    |Var'(VarFree(v)) -> 
      let address_in_vars = find_fvar_indx v fvars in
      let code =Printf.sprintf "mov rax, [fvar_tbl+%d*8]" address_in_vars in
      code     
        
    |LambdaSimple'(params, body) ->
      let create_env =
        let label_is_not_empty = (Labels.make_label "is_not_empty") in
        let label_is_empty = (Labels.make_label "is_empty") in
        let label_env_loop = (Labels.make_label "env_loop") in
        let label_params_loop = (Labels.make_label "params_loop") in
        let label_no_more_params = (Labels.make_label "no_more_params") in
        (concat_lines
           [
             "GET_ENV rbx";
             "mov rcx, 0";
             "cmp rbx, SOB_NIL_ADDRESS";
             (Printf.sprintf "jne %s" label_is_not_empty);
             "MALLOC rdx, 8";
             "mov qword[rdx], SOB_NIL_ADDRESS";
             (Printf.sprintf "jmp %s" label_is_empty);
             (Printf.sprintf "%s:" label_is_not_empty);
             "ENV_LENGTH rbx";
             "mov rdi, rcx";
             "inc rdi";
             "shl rdi, 3";
             "MALLOC rdx, rdi";
             (Printf.sprintf "%s:" label_env_loop);
             "shl rcx, 3";
             "mov rsi, rbx;Env";
             "add rsi, rcx;Env[i]";
             "sub rsi, 8;Env[i-1]";
             "mov r8, rdx;ExtEnv";
             "add r8, rcx;ExtEnv[i]";
             "mov r9, qword[rsi];r9 is the i'th rib";
             "mov qword[r8], r9; ExtEnv[i] = Env[i-1]";
             "shr rcx, 3";
             (Printf.sprintf "loop %s" label_env_loop);
             (Printf.sprintf "mov rcx, %d" (List.length params));
             "shl rcx, 3";
             "MALLOC rbx, rcx;rbx is the new rib";
             "shr rcx, 3";
             "cmp rcx, 0";
             Printf.sprintf "je %s" label_no_more_params;
             (Printf.sprintf "%s:" label_params_loop);
             "mov rdi, rcx";
             "dec rdi;rdi is the 0 based index of the current arg";
             "GET_ARG rsi, rdi";
             "mov qword[rbx + rdi], rsi";
             (Printf.sprintf "loop %s" label_params_loop);
             (Printf.sprintf "%s:" label_no_more_params);
             (Printf.sprintf "%s:" label_is_empty);
             "mov qword[rdx], rbx";
             ";;RDX IS THE EXTENV!!!"
           ]
        )
      in
      let label_code = Labels.make_label "Lcode" in
      let label_cont = Labels.make_label "Lcont" in
      let create_closure =
        Printf.sprintf "MAKE_CLOSURE(rax, rdx, %s)" label_code
      in
      (concat_lines
         [
           create_env;
           create_closure;
           (Printf.sprintf "jmp %s" label_cont);
           (Printf.sprintf "%s:" label_code);
           "push rbp";
           "mov rbp, rsp";
           (generate consts fvars body);
           "leave";
           "ret";
           (Printf.sprintf "%s:" label_cont);
         ]
      )
      
      
    |no_match -> raise_not_imp "generate" no_match exp'_to_string
  ;;
  
end;;

