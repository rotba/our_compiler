#use "code-gen.ml";;


let debug = false;;
let print_debug s= (Printf.printf ";;; debug \n%s\n" s);;
            
let file_to_string f =
  let ic = open_in f in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  s;;

let string_to_asts s = List.map Semantics.run_semantics
                         (Tag_Parser.tag_parse_expressions
                            (Reader.read_sexprs s));;

let primitive_names_to_labels = 
  (* ["boolean?", "is_boolean"; "float?", "is_float"; "integer?", "is_integer"; "pair?", "is_pair";
   *  "null?", "is_null"; "char?", "is_char"; "string?", "is_string";
   *  "procedure?", "is_procedure"; "symbol?", "is_symbol"; "string-length", "string_length";
   *  "string-ref", "string_ref"; "string-set!", "string_set"; "make-string", "make_string";
   *  "symbol->string", "symbol_to_string"; 
   *  "char->integer", "char_to_integer"; "integer->char", "integer_to_char"; "eq?", "is_eq";
   *  "+", "bin_add"; "*", "bin_mul"; "-", "bin_sub"; "/", "bin_div"; "<", "bin_lt"; "=", "bin_equ"] *)
  ["+", "bin_add";"cons", "cons";"car","car"]
;;

let make_prologue consts_tbl fvars_tbl =
  let make_primitive_closure (prim, label) =
    (* Adapt the addressing here to your fvar addressing scheme:
       This imlementation assumes fvars are offset from the base label fvar_tbl *)
"    MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, " ^ label  ^ ")
     mov [fvar_tbl+8*" ^  (string_of_int (List.assoc prim fvars_tbl)) ^ "], rax" in
  let constant_bytes (c, (a, s)) = s in
"
;;; All the macros and the scheme-object printing procedure
;;; are defined in compiler.s
%include \"compiler.s\"

section .bss
;;; This pointer is used to manage allocations on our heap.
malloc_pointer:
    resq 1

section .data
const_tbl:

" ^ (String.concat "\n" (List.map constant_bytes consts_tbl)) ^ "

;;; These macro definitions are required for the primitive
;;; definitions in the epilogue to work properly
%define SOB_VOID_ADDRESS const_tbl+" ^ (string_of_int (fst (List.assoc Void consts_tbl))) ^ "
%define SOB_NIL_ADDRESS const_tbl+" ^ (string_of_int (fst (List.assoc (Sexpr Nil) consts_tbl))) ^ "
%define SOB_FALSE_ADDRESS const_tbl+" ^ (string_of_int (fst (List.assoc (Sexpr (Bool false)) consts_tbl))) ^ "
%define SOB_TRUE_ADDRESS const_tbl+" ^ (string_of_int  (fst (List.assoc (Sexpr (Bool true)) consts_tbl))) ^ "

fvar_tbl:
" ^
  (* This line should be adapted to your fvar-addressing scheme. 
     I.e., if you use direct labeling, you should output them here. *)
  (String.concat "\n" (List.map (fun _ -> "dq T_UNDEFINED") fvars_tbl)) ^ "

global main
section .text
main:
    push rbp

    ;; set up the heap
    mov rdi, GB(4)
    call malloc
    mov [malloc_pointer], rax

    ;; Set up the dummy activation frame
    ;; The dummy return address is T_UNDEFINED
    ;; (which a is a macro for 0) so that returning
    ;; from the top level (which SHOULD NOT HAPPEN
    ;; AND IS A BUG) will cause a segfault.
    push 0
    push SOB_NIL_ADDRESS
    push qword T_UNDEFINED
    push rsp
    mov rbp,rsp

    ;; Set up the primitive stdlib fvars:
    ;; Since the primtive procedures are defined in assembly,
    ;; they are not generated by scheme (define ...) expressions.
    ;; This is where we emulate the missing (define ...) expressions
    ;; for all the primitive procedures.
" ^ (String.concat "\n" (List.map make_primitive_closure primitive_names_to_labels)) ^ "

user_code_fragment:
;;; The code you compiled will be catenated here.
;;; It will be executed immediately after the closures for 
;;; the primitive procedures are set up.

";;

(* You may populate this variable with a string containing the epilogue.
   You may load it from a file, you may write it here inline, 
   you may just add things to prims.s (which gets catenated with the epilogue variable).
   Whatever floats your boat. You just have to make sure all the required
   primitive procedures are implemented and included in the output assembly. *)
let epilogue = "";;

exception X_missing_input_file;;

try
  let () = if(debug) then (print_debug "got_here") in
  let infile = Sys.argv.(1) in
  let () = if(debug) then (print_debug "got_here") in
  let code =  (file_to_string infile) in
  let () = if(debug) then (print_debug "got_here") in
  let asts = string_to_asts code in
  let () = if(debug) then (print_debug "got_here") in
  let consts_tbl = Code_Gen.make_consts_tbl asts in
  let () = if(debug) then (print_debug "got_here") in
  let fvars_tbl = Code_Gen.make_fvars_tbl asts in
  let () = if(debug) then (print_debug "got_here") in
  let generate = Code_Gen.generate consts_tbl fvars_tbl in
  let () = if(debug) then (print_debug "got_here") in
  let code_fragment = String.concat "\n\n"
                        (List.map
                           (fun ast -> (generate ast) ^ "\n\tcall write_sob_if_not_void")
                           asts) in
  let () = if(debug) then (print_debug "got_here") in
  (* clean_exit contains instructions to clean the dummy stack
     and return exit code 0 ("all's well") from procedure main. *)
  let clean_exit = "\n\n\tmov rax, 0\n\tadd rsp, 4*8\n\tpop rbp\n\tret\n\n" in
  let () = if(debug) then (print_debug "got_here") in
  let provided_primitives = file_to_string "prims.s" in
  let () = if(debug) then (print_debug code_fragment) in
  print_string ((make_prologue consts_tbl fvars_tbl)  ^
                  code_fragment ^ clean_exit ^
                    provided_primitives ^ "\n" ^ epilogue)

with Invalid_argument(x) -> raise X_missing_input_file;;
