
;;; All the macros and the scheme-object printing procedure
;;; are defined in compiler.s
%include "compiler.s"

section .bss
;;; This pointer is used to manage allocations on our heap.
malloc_pointer:
    resq 1

section .data
const_tbl:
MAKE_BOOL(1)
MAKE_BOOL(0)
MAKE_NIL
MAKE_VOID
MAKE_LITERAL_INT(3)
MAKE_LITERAL_INT(2)
MAKE_LITERAL_INT(1)
MAKE_LITERAL_INT(0)
MAKE_LITERAL_STRING "whatever", 8
MAKE_LITERAL_SYMBOl(const_tbl+42)

;;; These macro definitions are required for the primitive
;;; definitions in the epilogue to work properly
%define SOB_VOID_ADDRESS const_tbl+5
%define SOB_NIL_ADDRESS const_tbl+4
%define SOB_FALSE_ADDRESS const_tbl+2
%define SOB_TRUE_ADDRESS const_tbl+0

fvar_tbl:
dq T_UNDEFINED
dq T_UNDEFINED
dq T_UNDEFINED
dq T_UNDEFINED
dq T_UNDEFINED
dq T_UNDEFINED
dq T_UNDEFINED
dq T_UNDEFINED
dq T_UNDEFINED
dq T_UNDEFINED
dq T_UNDEFINED
dq T_UNDEFINED
dq T_UNDEFINED
dq T_UNDEFINED
dq T_UNDEFINED
dq T_UNDEFINED
dq T_UNDEFINED
dq T_UNDEFINED
dq T_UNDEFINED
dq T_UNDEFINED
dq T_UNDEFINED
dq T_UNDEFINED
dq T_UNDEFINED
dq T_UNDEFINED
dq T_UNDEFINED
dq T_UNDEFINED
dq T_UNDEFINED
dq T_UNDEFINED
dq T_UNDEFINED
dq T_UNDEFINED

global main
extern memmove
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
    push -1
    push rsp
    mov rbp,rsp

    ;; Set up the primitive stdlib fvars:
    ;; Since the primtive procedures are defined in assembly,
    ;; they are not generated by scheme (define ...) expressions.
    ;; This is where we emulate the missing (define ...) expressions
    ;; for all the primitive procedures.
    MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, is_boolean)
    mov [fvar_tbl+8*0], rax
    MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, is_float)
    mov [fvar_tbl+8*1], rax
    MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, is_integer)
    mov [fvar_tbl+8*2], rax
    MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, is_pair)
    mov [fvar_tbl+8*3], rax
    MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, is_null)
    mov [fvar_tbl+8*4], rax
    MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, is_char)
    mov [fvar_tbl+8*5], rax
    MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, is_string)
    mov [fvar_tbl+8*6], rax
    MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, is_procedure)
    mov [fvar_tbl+8*7], rax
    MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, is_symbol)
    mov [fvar_tbl+8*8], rax
    MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, string_length)
    mov [fvar_tbl+8*9], rax
    MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, string_ref)
    mov [fvar_tbl+8*10], rax
    MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, string_set)
    mov [fvar_tbl+8*11], rax
    MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, make_string)
    mov [fvar_tbl+8*12], rax
    MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, symbol_to_string)
    mov [fvar_tbl+8*13], rax
    MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, char_to_integer)
    mov [fvar_tbl+8*14], rax
    MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, integer_to_char)
    mov [fvar_tbl+8*15], rax
    MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, is_eq)
    mov [fvar_tbl+8*16], rax
    MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, bin_add)
    mov [fvar_tbl+8*17], rax
    MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, bin_mul)
    mov [fvar_tbl+8*18], rax
    MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, bin_sub)
    mov [fvar_tbl+8*19], rax
    MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, bin_div)
    mov [fvar_tbl+8*20], rax
    MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, bin_lt)
    mov [fvar_tbl+8*21], rax
    MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, bin_equ)
    mov [fvar_tbl+8*22], rax
    MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, cons)
    mov [fvar_tbl+8*23], rax
    MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, car)
    mov [fvar_tbl+8*24], rax
    MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, cdr)
    mov [fvar_tbl+8*25], rax
    MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, apply)
    mov [fvar_tbl+8*28], rax
    MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, set_car)
    mov [fvar_tbl+8*26], rax
    MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, set_cdr)
    mov [fvar_tbl+8*27], rax

user_code_fragment:
;;; The code you compiled will be catenated here.
;;; It will be executed immediately after the closures for 
;;; the primitive procedures are set up.

push 496351

mov rax, qword[fvar_tbl+25*8]
push rax
mov rax, qword[fvar_tbl+24*8]
push rax
mov rax, qword[fvar_tbl+4*8]
push rax
push 3
;(LambdaSimple'(  ( null? , car , cdr ) , [ApplicTP'( (LambdaSimple'(  ( fold-loop ) , [(Seq'( [(Set'( Var' ( VarParam ( fold-loop , 0 )) , (Box'( VarParam ( fold-loop , 0 ) ) ) , (Seq'( [(BoxSet'( VarParam ( fold-loop , 0 ) , (LambdaSimple'(  ( f , acc , l ) , [(If'( Applic'( Var' ( VarBound ( null? , 1 , 1 )) , [Var' ( VarParam ( l , 2 ))] ) , Var' ( VarParam ( acc , 1 )),ApplicTP'( (BoxGet'( VarBound ( fold-loop , 0 , 0 ) ) , [Var' ( VarParam ( f , 0 )) , Applic'( Var' ( VarParam ( f , 0 )) , [Var' ( VarParam ( acc , 1 )) , Applic'( Var' ( VarBound ( car , 1 , 1 )) , [Var' ( VarParam ( l , 2 ))] )] ) , Applic'( Var' ( VarBound ( cdr , 1 , 1 )) , [Var' ( VarParam ( l , 2 ))] )] ) )] ) ) , (BoxGet'( VarParam ( fold-loop , 0 ) )] )] )] ) , [Const' ( Symbol(whatever))] )] )
GET_ENV rbx
mov rcx, 0
cmp rbx, SOB_NIL_ADDRESS
jne is_not_empty_26
MALLOC rdx, 8
mov qword[rdx], SOB_NIL_ADDRESS
jmp is_empty_27
is_not_empty_26:
ENV_LENGTH rbx
mov rdi, rcx
inc rdi
shl rdi, 3
MALLOC rdx, rdi
env_loop_28:
shl rcx, 3
mov rsi, rbx;Env
add rsi, rcx;Env[i]
sub rsi, 8;Env[i-1]
mov r8, rdx;ExtEnv
add r8, rcx;ExtEnv[i]
mov r9, qword[rsi];r9 is the i'th rib
mov qword[r8], r9; ExtEnv[i] = Env[i-1]
shr rcx, 3
loop env_loop_28
mov rcx, qword[rbp +8*3]
shl rcx, 3
MALLOC rbx, rcx;rbx is the new rib
shr rcx, 3
cmp rcx, 0
je no_more_params_30
params_loop_29:
mov rdi, rcx
dec rdi;rdi is the 0 based index of the current arg
GET_ARG rsi, rdi
mov qword[rbx + rdi*8], rsi
loop params_loop_29
no_more_params_30:
mov qword[rdx], rbx
is_empty_27:
;;RDX IS THE EXTENV!!!
MAKE_CLOSURE(rax, rdx, Lcode_31)
jmp Lcont_32
Lcode_31:
push rbp
mov rbp, rsp
push 496351

mov rax, const_tbl+59
push rax
push 1
;(LambdaSimple'(  ( fold-loop ) , [(Seq'( [(Set'( Var' ( VarParam ( fold-loop , 0 )) , (Box'( VarParam ( fold-loop , 0 ) ) ) , (Seq'( [(BoxSet'( VarParam ( fold-loop , 0 ) , (LambdaSimple'(  ( f , acc , l ) , [(If'( Applic'( Var' ( VarBound ( null? , 1 , 1 )) , [Var' ( VarParam ( l , 2 ))] ) , Var' ( VarParam ( acc , 1 )),ApplicTP'( (BoxGet'( VarBound ( fold-loop , 0 , 0 ) ) , [Var' ( VarParam ( f , 0 )) , Applic'( Var' ( VarParam ( f , 0 )) , [Var' ( VarParam ( acc , 1 )) , Applic'( Var' ( VarBound ( car , 1 , 1 )) , [Var' ( VarParam ( l , 2 ))] )] ) , Applic'( Var' ( VarBound ( cdr , 1 , 1 )) , [Var' ( VarParam ( l , 2 ))] )] ) )] ) ) , (BoxGet'( VarParam ( fold-loop , 0 ) )] )] )] )
GET_ENV rbx
mov rcx, 0
cmp rbx, SOB_NIL_ADDRESS
jne is_not_empty_43
MALLOC rdx, 8
mov qword[rdx], SOB_NIL_ADDRESS
jmp is_empty_44
is_not_empty_43:
ENV_LENGTH rbx
mov rdi, rcx
inc rdi
shl rdi, 3
MALLOC rdx, rdi
env_loop_45:
shl rcx, 3
mov rsi, rbx;Env
add rsi, rcx;Env[i]
sub rsi, 8;Env[i-1]
mov r8, rdx;ExtEnv
add r8, rcx;ExtEnv[i]
mov r9, qword[rsi];r9 is the i'th rib
mov qword[r8], r9; ExtEnv[i] = Env[i-1]
shr rcx, 3
loop env_loop_45
mov rcx, qword[rbp +8*3]
shl rcx, 3
MALLOC rbx, rcx;rbx is the new rib
shr rcx, 3
cmp rcx, 0
je no_more_params_47
params_loop_46:
mov rdi, rcx
dec rdi;rdi is the 0 based index of the current arg
GET_ARG rsi, rdi
mov qword[rbx + rdi*8], rsi
loop params_loop_46
no_more_params_47:
mov qword[rdx], rbx
is_empty_44:
;;RDX IS THE EXTENV!!!
MAKE_CLOSURE(rax, rdx, Lcode_48)
jmp Lcont_49
Lcode_48:
push rbp
mov rbp, rsp
mov rax, qword [rbp+8*(4+0)]
MALLOC rbx, 8
mov qword[rbx], rax
mov rax, rbx
mov qword [rbp+8*(4+0)], rax
mov rax, SOB_VOID_ADDRESS
GET_ENV rbx
mov rcx, 0
cmp rbx, SOB_NIL_ADDRESS
jne is_not_empty_85
MALLOC rdx, 8
mov qword[rdx], SOB_NIL_ADDRESS
jmp is_empty_86
is_not_empty_85:
ENV_LENGTH rbx
mov rdi, rcx
inc rdi
shl rdi, 3
MALLOC rdx, rdi
env_loop_87:
shl rcx, 3
mov rsi, rbx;Env
add rsi, rcx;Env[i]
sub rsi, 8;Env[i-1]
mov r8, rdx;ExtEnv
add r8, rcx;ExtEnv[i]
mov r9, qword[rsi];r9 is the i'th rib
mov qword[r8], r9; ExtEnv[i] = Env[i-1]
shr rcx, 3
loop env_loop_87
mov rcx, qword[rbp +8*3]
shl rcx, 3
MALLOC rbx, rcx;rbx is the new rib
shr rcx, 3
cmp rcx, 0
je no_more_params_89
params_loop_88:
mov rdi, rcx
dec rdi;rdi is the 0 based index of the current arg
GET_ARG rsi, rdi
mov qword[rbx + rdi*8], rsi
loop params_loop_88
no_more_params_89:
mov qword[rdx], rbx
is_empty_86:
;;RDX IS THE EXTENV!!!
MAKE_CLOSURE(rax, rdx, Lcode_90)
jmp Lcont_91
Lcode_90:
push rbp
mov rbp, rsp
push 496351

mov rax, qword [rbp+8*(4+2)]
push rax
push 1
;Var' ( VarBound ( null? , 1 , 1 ))
mov rax, qword[rbp+8*2]
mov rax, qword[rax+8*1]
mov rax, qword[rax+8*0]

CLOSURE_ENV rbx, rax
push rbx
CLOSURE_CODE rbx, rax
call rbx
add rsp, 8*1
pop rbx
inc rbx
shl rbx, 3
add rsp, rbx
cmp rax, SOB_FALSE_ADDRESS
je Lelse_182
mov rax, qword [rbp+8*(4+1)]
jmp Lexit_183
Lelse_182:
push 496351

push 496351

mov rax, qword [rbp+8*(4+2)]
push rax
push 1
;Var' ( VarBound ( cdr , 1 , 1 ))
mov rax, qword[rbp+8*2]
mov rax, qword[rax+8*1]
mov rax, qword[rax+8*2]

CLOSURE_ENV rbx, rax
push rbx
CLOSURE_CODE rbx, rax
call rbx
add rsp, 8*1
pop rbx
inc rbx
shl rbx, 3
add rsp, rbx
push rax
push 496351

push 496351

mov rax, qword [rbp+8*(4+2)]
push rax
push 1
;Var' ( VarBound ( car , 1 , 1 ))
mov rax, qword[rbp+8*2]
mov rax, qword[rax+8*1]
mov rax, qword[rax+8*1]

CLOSURE_ENV rbx, rax
push rbx
CLOSURE_CODE rbx, rax
call rbx
add rsp, 8*1
pop rbx
inc rbx
shl rbx, 3
add rsp, rbx
push rax
mov rax, qword [rbp+8*(4+1)]
push rax
push 2
;Var' ( VarParam ( f , 0 ))
mov rax, qword [rbp+8*(4+0)]

CLOSURE_ENV rbx, rax
push rbx
CLOSURE_CODE rbx, rax
call rbx
add rsp, 8*1
pop rbx
inc rbx
shl rbx, 3
add rsp, rbx
push rax
mov rax, qword [rbp+8*(4+0)]
push rax
push 3
;(BoxGet'( VarBound ( fold-loop , 0 , 0 ) )
mov rax, qword[rbp+8*2]
mov rax, qword[rax+8*0]
mov rax, qword[rax+8*0]
mov rax, qword[rax]

CLOSURE_ENV rbx, rax
push rbx
push qword[rbp +8*1]
SHIFT_FRAME 7
CLOSURE_CODE rbx, rax
jmp rbx
add rsp, 8*1
pop rbx
inc rbx
shl rbx, 3
add rsp, rbx
Lexit_183:
leave
ret
Lcont_91:
push rax
mov rax, qword [rbp+8*(4+0)]
pop qword[rax]
mov rax, SOB_VOID_ADDRESS
mov rax, qword [rbp+8*(4+0)]
mov rax, qword[rax]
leave
ret
Lcont_49:

CLOSURE_ENV rbx, rax
push rbx
push qword[rbp +8*1]
SHIFT_FRAME 5
CLOSURE_CODE rbx, rax
jmp rbx
add rsp, 8*1
pop rbx
inc rbx
shl rbx, 3
add rsp, rbx
leave
ret
Lcont_32:

CLOSURE_ENV rbx, rax
push rbx
CLOSURE_CODE rbx, rax
call rbx
add rsp, 8*1
pop rbx
inc rbx
shl rbx, 3
add rsp, rbx
mov qword[fvar_tbl+29*8], rax 
mov rax, SOB_VOID_ADDRESS

	call write_sob_if_not_void

push 496351

mov rax, qword[fvar_tbl+17*8]
push rax
mov rax, qword[fvar_tbl+29*8]
push rax
push 2
;(LambdaSimple'(  ( fold-left , + ) , [(LambdaOpt'(  (  . x ), , [ApplicTP'( Var' ( VarBound ( fold-left , 0 , 0 )) , [Var' ( VarBound ( + , 0 , 0 )) , Const' ( 0) , Var' ( VarParam ( x , 0 ))] )] )] )
GET_ENV rbx
mov rcx, 0
cmp rbx, SOB_NIL_ADDRESS
jne is_not_empty_214
MALLOC rdx, 8
mov qword[rdx], SOB_NIL_ADDRESS
jmp is_empty_215
is_not_empty_214:
ENV_LENGTH rbx
mov rdi, rcx
inc rdi
shl rdi, 3
MALLOC rdx, rdi
env_loop_216:
shl rcx, 3
mov rsi, rbx;Env
add rsi, rcx;Env[i]
sub rsi, 8;Env[i-1]
mov r8, rdx;ExtEnv
add r8, rcx;ExtEnv[i]
mov r9, qword[rsi];r9 is the i'th rib
mov qword[r8], r9; ExtEnv[i] = Env[i-1]
shr rcx, 3
loop env_loop_216
mov rcx, qword[rbp +8*3]
shl rcx, 3
MALLOC rbx, rcx;rbx is the new rib
shr rcx, 3
cmp rcx, 0
je no_more_params_218
params_loop_217:
mov rdi, rcx
dec rdi;rdi is the 0 based index of the current arg
GET_ARG rsi, rdi
mov qword[rbx + rdi*8], rsi
loop params_loop_217
no_more_params_218:
mov qword[rdx], rbx
is_empty_215:
;;RDX IS THE EXTENV!!!
MAKE_CLOSURE(rax, rdx, Lcode_219)
jmp Lcont_220
Lcode_219:
push rbp
mov rbp, rsp
GET_ENV rbx
mov rcx, 0
cmp rbx, SOB_NIL_ADDRESS
jne is_not_empty_221
MALLOC rdx, 8
mov qword[rdx], SOB_NIL_ADDRESS
jmp is_empty_222
is_not_empty_221:
ENV_LENGTH rbx
mov rdi, rcx
inc rdi
shl rdi, 3
MALLOC rdx, rdi
env_loop_223:
shl rcx, 3
mov rsi, rbx;Env
add rsi, rcx;Env[i]
sub rsi, 8;Env[i-1]
mov r8, rdx;ExtEnv
add r8, rcx;ExtEnv[i]
mov r9, qword[rsi];r9 is the i'th rib
mov qword[r8], r9; ExtEnv[i] = Env[i-1]
shr rcx, 3
loop env_loop_223
mov rcx, qword[rbp +8*3]
shl rcx, 3
MALLOC rbx, rcx;rbx is the new rib
shr rcx, 3
cmp rcx, 0
je no_more_params_225
params_loop_224:
mov rdi, rcx
dec rdi;rdi is the 0 based index of the current arg
GET_ARG rsi, rdi
mov qword[rbx + rdi*8], rsi
loop params_loop_224
no_more_params_225:
mov qword[rdx], rbx
is_empty_222:
;;RDX IS THE EXTENV!!!
MAKE_CLOSURE(rax, rdx, Lcode_226)
jmp Lcont_227
Lcode_226:
;donte the effective numer of parameters m
;donte the noumber of simple parameters n
mov rcx, qword[rsp+ 8*2]; rcx is m
cmp rcx, 0
jne not_empty_opt_228
; m = n
add rcx, 3;m+2 - offset of magic
shl rcx, 3
mov qword[rsp+rcx], SOB_NIL_ADDRESS; magic is NIL
jmp done_fixing_229
not_empty_opt_228:
add rcx, 2; rcx is m+2 - the offset of the ultimetly last argument
mov rdi, rcx
mov rbx, qword[rsp+8*rdi]
MAKE_PAIR(rdx, rbx, SOB_NIL_ADDRESS)
mov qword[rsp+8*rdi], rdx;Arg_m-1 contains '(Arg_n-1)
sub rcx, 2; rcx is m
sub rcx, 0; rcx is m-n
dec rcx; because we've already handled the top opt param
cmp rcx, 0
je done_fixing_229
;rcx is the number of optional parameters left (i.e. (m-n)-1)
create_opt_loop_230:
mov rdx, rcx; rdx is curr_m (i.e: (m-n) - i, i.e the current amount of not consumed optional args left)
dec rdx; rdx is now offset
add rdx, 3; offset of arg_0+(curr_m-1)
add rdx, 0; offset of last optional param not consumed
mov rbx, qword[rsp + 8*rdx]
mov rdi, qword[rsp + 8*(rdx +1)]
MAKE_PAIR(rsi, rbx, rdi)
mov qword[rsp + 8*(rdx +1)], rsi
mov rax, 0
mov rdi, rsp
add rdi, 8; destination
mov rsi, rsp;source
mov rdx, 0;n
add rdx, 3; n+3
add rdx, rcx;n+3+curr_m
dec rdx; because the last opttion param have been consumed
shl rdx, 3
push rcx
call memmove
pop rcx
add rsp, 8
sub qword[rsp +2*8], 1; curr_m = curr_m-1
loop create_opt_loop_230
done_fixing_229:
push rbp
mov rbp, rsp
push 496351

mov rax, qword [rbp+8*(4+0)]
push rax
mov rax, const_tbl+33
push rax
mov rax, qword[rbp+8*2]
mov rax, qword[rax+8*0]
mov rax, qword[rax+8*1]
push rax
push 3
;Var' ( VarBound ( fold-left , 0 , 0 ))
mov rax, qword[rbp+8*2]
mov rax, qword[rax+8*0]
mov rax, qword[rax+8*0]

CLOSURE_ENV rbx, rax
push rbx
push qword[rbp +8*1]
SHIFT_FRAME 7
CLOSURE_CODE rbx, rax
jmp rbx
add rsp, 8*1
pop rbx
inc rbx
shl rbx, 3
add rsp, rbx
leave
ret
Lcont_227:
leave
ret
Lcont_220:

CLOSURE_ENV rbx, rax
push rbx
CLOSURE_CODE rbx, rax
call rbx
add rsp, 8*1
pop rbx
inc rbx
shl rbx, 3
add rsp, rbx
mov qword[fvar_tbl+17*8], rax 
mov rax, SOB_VOID_ADDRESS

	call write_sob_if_not_void

push 496351

mov rax, qword[fvar_tbl+4*8]
push rax
mov rax, qword[fvar_tbl+17*8]
push rax
mov rax, qword[fvar_tbl+19*8]
push rax
mov rax, qword[fvar_tbl+28*8]
push rax
push 4
;(LambdaSimple'(  ( apply , - , + , null? ) , [(LambdaOpt'(  ( x . y ), , [(If'( Applic'( Var' ( VarBound ( null? , 0 , 0 )) , [Var' ( VarParam ( y , 1 ))] ) , ApplicTP'( Var' ( VarBound ( - , 0 , 0 )) , [Const' ( 0) , Var' ( VarParam ( x , 0 ))] ),ApplicTP'( Var' ( VarBound ( - , 0 , 0 )) , [Var' ( VarParam ( x , 0 )) , Applic'( Var' ( VarBound ( apply , 0 , 0 )) , [Var' ( VarBound ( + , 0 , 0 )) , Var' ( VarParam ( y , 1 ))] )] ) )] )] )
GET_ENV rbx
mov rcx, 0
cmp rbx, SOB_NIL_ADDRESS
jne is_not_empty_286
MALLOC rdx, 8
mov qword[rdx], SOB_NIL_ADDRESS
jmp is_empty_287
is_not_empty_286:
ENV_LENGTH rbx
mov rdi, rcx
inc rdi
shl rdi, 3
MALLOC rdx, rdi
env_loop_288:
shl rcx, 3
mov rsi, rbx;Env
add rsi, rcx;Env[i]
sub rsi, 8;Env[i-1]
mov r8, rdx;ExtEnv
add r8, rcx;ExtEnv[i]
mov r9, qword[rsi];r9 is the i'th rib
mov qword[r8], r9; ExtEnv[i] = Env[i-1]
shr rcx, 3
loop env_loop_288
mov rcx, qword[rbp +8*3]
shl rcx, 3
MALLOC rbx, rcx;rbx is the new rib
shr rcx, 3
cmp rcx, 0
je no_more_params_290
params_loop_289:
mov rdi, rcx
dec rdi;rdi is the 0 based index of the current arg
GET_ARG rsi, rdi
mov qword[rbx + rdi*8], rsi
loop params_loop_289
no_more_params_290:
mov qword[rdx], rbx
is_empty_287:
;;RDX IS THE EXTENV!!!
MAKE_CLOSURE(rax, rdx, Lcode_291)
jmp Lcont_292
Lcode_291:
push rbp
mov rbp, rsp
GET_ENV rbx
mov rcx, 0
cmp rbx, SOB_NIL_ADDRESS
jne is_not_empty_293
MALLOC rdx, 8
mov qword[rdx], SOB_NIL_ADDRESS
jmp is_empty_294
is_not_empty_293:
ENV_LENGTH rbx
mov rdi, rcx
inc rdi
shl rdi, 3
MALLOC rdx, rdi
env_loop_295:
shl rcx, 3
mov rsi, rbx;Env
add rsi, rcx;Env[i]
sub rsi, 8;Env[i-1]
mov r8, rdx;ExtEnv
add r8, rcx;ExtEnv[i]
mov r9, qword[rsi];r9 is the i'th rib
mov qword[r8], r9; ExtEnv[i] = Env[i-1]
shr rcx, 3
loop env_loop_295
mov rcx, qword[rbp +8*3]
shl rcx, 3
MALLOC rbx, rcx;rbx is the new rib
shr rcx, 3
cmp rcx, 0
je no_more_params_297
params_loop_296:
mov rdi, rcx
dec rdi;rdi is the 0 based index of the current arg
GET_ARG rsi, rdi
mov qword[rbx + rdi*8], rsi
loop params_loop_296
no_more_params_297:
mov qword[rdx], rbx
is_empty_294:
;;RDX IS THE EXTENV!!!
MAKE_CLOSURE(rax, rdx, Lcode_298)
jmp Lcont_299
Lcode_298:
;donte the effective numer of parameters m
;donte the noumber of simple parameters n
mov rcx, qword[rsp+ 8*2]; rcx is m
cmp rcx, 1
jne not_empty_opt_300
; m = n
add rcx, 3;m+2 - offset of magic
shl rcx, 3
mov qword[rsp+rcx], SOB_NIL_ADDRESS; magic is NIL
jmp done_fixing_301
not_empty_opt_300:
add rcx, 2; rcx is m+2 - the offset of the ultimetly last argument
mov rdi, rcx
mov rbx, qword[rsp+8*rdi]
MAKE_PAIR(rdx, rbx, SOB_NIL_ADDRESS)
mov qword[rsp+8*rdi], rdx;Arg_m-1 contains '(Arg_n-1)
sub rcx, 2; rcx is m
sub rcx, 1; rcx is m-n
dec rcx; because we've already handled the top opt param
cmp rcx, 0
je done_fixing_301
;rcx is the number of optional parameters left (i.e. (m-n)-1)
create_opt_loop_302:
mov rdx, rcx; rdx is curr_m (i.e: (m-n) - i, i.e the current amount of not consumed optional args left)
dec rdx; rdx is now offset
add rdx, 3; offset of arg_0+(curr_m-1)
add rdx, 1; offset of last optional param not consumed
mov rbx, qword[rsp + 8*rdx]
mov rdi, qword[rsp + 8*(rdx +1)]
MAKE_PAIR(rsi, rbx, rdi)
mov qword[rsp + 8*(rdx +1)], rsi
mov rax, 0
mov rdi, rsp
add rdi, 8; destination
mov rsi, rsp;source
mov rdx, 1;n
add rdx, 3; n+3
add rdx, rcx;n+3+curr_m
dec rdx; because the last opttion param have been consumed
shl rdx, 3
push rcx
call memmove
pop rcx
add rsp, 8
sub qword[rsp +2*8], 1; curr_m = curr_m-1
loop create_opt_loop_302
done_fixing_301:
push rbp
mov rbp, rsp
push 496351

mov rax, qword [rbp+8*(4+1)]
push rax
push 1
;Var' ( VarBound ( null? , 0 , 0 ))
mov rax, qword[rbp+8*2]
mov rax, qword[rax+8*0]
mov rax, qword[rax+8*3]

CLOSURE_ENV rbx, rax
push rbx
CLOSURE_CODE rbx, rax
call rbx
add rsp, 8*1
pop rbx
inc rbx
shl rbx, 3
add rsp, rbx
cmp rax, SOB_FALSE_ADDRESS
je Lelse_378
push 496351

mov rax, qword [rbp+8*(4+0)]
push rax
mov rax, const_tbl+33
push rax
push 2
;Var' ( VarBound ( - , 0 , 0 ))
mov rax, qword[rbp+8*2]
mov rax, qword[rax+8*0]
mov rax, qword[rax+8*1]

CLOSURE_ENV rbx, rax
push rbx
push qword[rbp +8*1]
SHIFT_FRAME 6
CLOSURE_CODE rbx, rax
jmp rbx
add rsp, 8*1
pop rbx
inc rbx
shl rbx, 3
add rsp, rbx
jmp Lexit_379
Lelse_378:
push 496351

push 496351

mov rax, qword [rbp+8*(4+1)]
push rax
mov rax, qword[rbp+8*2]
mov rax, qword[rax+8*0]
mov rax, qword[rax+8*2]
push rax
push 2
;Var' ( VarBound ( apply , 0 , 0 ))
mov rax, qword[rbp+8*2]
mov rax, qword[rax+8*0]
mov rax, qword[rax+8*0]

CLOSURE_ENV rbx, rax
push rbx
CLOSURE_CODE rbx, rax
call rbx
add rsp, 8*1
pop rbx
inc rbx
shl rbx, 3
add rsp, rbx
push rax
mov rax, qword [rbp+8*(4+0)]
push rax
push 2
;Var' ( VarBound ( - , 0 , 0 ))
mov rax, qword[rbp+8*2]
mov rax, qword[rax+8*0]
mov rax, qword[rax+8*1]

CLOSURE_ENV rbx, rax
push rbx
push qword[rbp +8*1]
SHIFT_FRAME 6
CLOSURE_CODE rbx, rax
jmp rbx
add rsp, 8*1
pop rbx
inc rbx
shl rbx, 3
add rsp, rbx
Lexit_379:
leave
ret
Lcont_299:
leave
ret
Lcont_292:

CLOSURE_ENV rbx, rax
push rbx
CLOSURE_CODE rbx, rax
call rbx
add rsp, 8*1
pop rbx
inc rbx
shl rbx, 3
add rsp, rbx
mov qword[fvar_tbl+19*8], rax 
mov rax, SOB_VOID_ADDRESS

	call write_sob_if_not_void

mov rax, const_tbl+24
cmp rax, SOB_FALSE_ADDRESS 
 jne Lexit_385 
mov rax, const_tbl+15
cmp rax, SOB_FALSE_ADDRESS 
 jne Lexit_385 
mov rax, const_tbl+6
Lexit_385:

	call write_sob_if_not_void

	mov rax, 0
	add rsp, 4*8
	pop rbp
	ret

apply:
;;; (apply proc x0 ... xn-1 s)
;;; s id s_0 .. s_m-1

;;; TODO: add closure verification
	push rbp
	mov rbp, rsp
	push 496351
	mov rbx, qword[rbp+8*3]
	add rbx, 3
	mov rsi, qword[rbp + 8*rbx]	; rsi is s
	mov rdi, 0		;the length of s
push_s_loop:
	cmp rsi, SOB_NIL_ADDRESS
	je end_push_s
	inc rdi
	CAR rbx, rsi
	push rbx
	CDR rsi, rsi
	jmp push_s_loop
end_push_s:
;;; magic s0 ... sm-1 on the stack
	cmp rdi, 0
	je reverse_s_end
	mov rcx, rdi
	cmp rcx ,1
	jbe reverse_s_end
	shr rcx,1 		;rcx is m/2
reverse_s_loop:
	mov rdx, rcx
	dec rdx			    ;rdx is the index (e.g 0 based)
	mov rbx, qword[rsp + rdx*8] ;rbx is s_i
	mov r8, rdi
	dec r8			  ;r8 is the index of le last elemnt
	sub r8, rdx		;m-i
	mov r9, qword[rsp + r8*8] ; r9 is s_m-i
	mov qword[rsp + rdx*8], r9 
	mov qword[rsp + r8*8], rbx
	loop reverse_s_loop
reverse_s_end:
	mov rcx, qword[rbp+8*3]
	sub rcx, 2		;rdx is n
	mov rsi, rcx		;save it for later
	cmp rsi, 0
	je no_params
push_xs_loop:
	mov rdx, rcx
	dec rdx			;rdx is index
	add rdx, 5		;5  and not 4 because the first parameter is proc
	push qword[rbp +8*rdx]
	loop push_xs_loop
no_params:	
;;; magic s_m-1,..,s_0, x_n-1,..x_0 on the stack
	add rdi, rsi		  ;m+n
	push rdi
	mov rbx, qword[rbp + 8*4] ; rbx is proc
;;; verify that proc is (i.e rbx) a closue
	CLOSURE_ENV rdx, rbx
	push rdx
	mov r8, rbx		;save the closure
	push qword[rbp+8]	;ret address
	mov rdx, rsp 		;this is the source of the memmory move
	push r8
	mov rbx, GET_PARAM_COUNT		;calc memmory length
	add rbx, ELEMENTS_ON_STACK ;;cacl memmory length
	shl rbx, 3			  ;rbx is the number of bytes need to be moved
	mov rbp, qword[rbp]	;current rbp is no longer needed and old rbp need to be accesibl
	mov rsi, rdx
	add rsi, rbx		;src is dest + param_c +elements_on_stack
	mov rax, 0
	mov rdi, rsi		;dest
	mov rsi, rdx		;src
	mov rdx, rbx		;len
	call memmove
	pop rbx			;proc
	mov rsp, rax
	CLOSURE_CODE rdx, rbx
	jmp rdx
	

	
	
	
	
	
	
	
cons:
	push rbp
	mov rbp, rsp
	GET_ARG rsi, 0
	GET_ARG rdx, 1
	MAKE_PAIR(rax, rsi, rdx)
	leave
	ret
car:
	push rbp
	mov rbp, rsp
	GET_ARG rsi, 0
	CAR rax, rsi
	leave
	ret

set_car:
	push rbp
	mov rbp, rsp
	GET_ARG rsi, 0
	GET_ARG rdi, 1
	mov qword[rsi+TYPE_SIZE], rdi
	mov rax, SOB_VOID_ADDRESS
	leave
	ret

set_cdr:
	push rbp
	mov rbp, rsp
	GET_ARG rsi, 0
	GET_ARG rdi, 1
	mov qword[rsi+TYPE_SIZE+WORD_SIZE], rdi
	mov rax, SOB_VOID_ADDRESS
	leave
	ret

cdr:
	push rbp
	mov rbp, rsp
	GET_ARG rsi, 0
	CDR rax, rsi
	leave
	ret
is_boolean:
    push rbp
    mov rbp, rsp

    mov rsi, PVAR(0)
    mov sil, byte [rsi]

    cmp sil, T_BOOL
    jne .wrong_type
    mov rax, SOB_TRUE_ADDRESS
    jmp .return

.wrong_type:
    mov rax, SOB_FALSE_ADDRESS
.return:
    leave
    ret

is_float:
    push rbp
    mov rbp, rsp

    mov rsi, PVAR(0)
    mov sil, byte [rsi]

    cmp sil, T_FLOAT
    jne .wrong_type
    mov rax, SOB_TRUE_ADDRESS
    jmp .return

.wrong_type:
    mov rax, SOB_FALSE_ADDRESS
.return:
    leave
    ret

is_integer:
    push rbp
    mov rbp, rsp

    mov rsi, PVAR(0)
    mov sil, byte [rsi]

    cmp sil, T_INTEGER
    jne .wrong_type
    mov rax, SOB_TRUE_ADDRESS
    jmp .return

.wrong_type:
    mov rax, SOB_FALSE_ADDRESS
.return:
    leave
    ret

is_pair:
    push rbp
    mov rbp, rsp

    mov rsi, PVAR(0)
    mov sil, byte [rsi]

    cmp sil, T_PAIR
    jne .wrong_type
    mov rax, SOB_TRUE_ADDRESS
    jmp .return

.wrong_type:
    mov rax, SOB_FALSE_ADDRESS
.return:
    leave
    ret

is_null:
    push rbp
    mov rbp, rsp

    mov rsi, PVAR(0)
    mov sil, byte [rsi]

    cmp sil, T_NIL
    jne .wrong_type
    mov rax, SOB_TRUE_ADDRESS
    jmp .return

.wrong_type:
    mov rax, SOB_FALSE_ADDRESS
.return:
    leave
    ret

is_char:
    push rbp
    mov rbp, rsp

    mov rsi, PVAR(0)
    mov sil, byte [rsi]

    cmp sil, T_CHAR
    jne .wrong_type
    mov rax, SOB_TRUE_ADDRESS
    jmp .return

.wrong_type:
    mov rax, SOB_FALSE_ADDRESS
.return:
    leave
    ret

is_string:
    push rbp
    mov rbp, rsp

    mov rsi, PVAR(0)
    mov sil, byte [rsi]

    cmp sil, T_STRING
    jne .wrong_type
    mov rax, SOB_TRUE_ADDRESS
    jmp .return

.wrong_type:
    mov rax, SOB_FALSE_ADDRESS
.return:
    leave
    ret

is_procedure:
    push rbp
    mov rbp, rsp

    mov rsi, PVAR(0)
    mov sil, byte [rsi]

    cmp sil, T_CLOSURE
    jne .wrong_type
    mov rax, SOB_TRUE_ADDRESS
    jmp .return

.wrong_type:
    mov rax, SOB_FALSE_ADDRESS
.return:
    leave
    ret

is_symbol:
    push rbp
    mov rbp, rsp

    mov rsi, PVAR(0)
    mov sil, byte [rsi]

    cmp sil, T_SYMBOL
    jne .wrong_type
    mov rax, SOB_TRUE_ADDRESS
    jmp .return

.wrong_type:
    mov rax, SOB_FALSE_ADDRESS
.return:
    leave
    ret

string_length:
    push rbp
    mov rbp, rsp

    mov rsi, PVAR(0)
    STRING_LENGTH rsi, rsi
    MAKE_INT(rax, rsi)

    leave
    ret

string_ref:
    push rbp
    mov rbp, rsp

    mov rsi, PVAR(0) 
    STRING_ELEMENTS rsi, rsi
    mov rdi, PVAR(1)
    INT_VAL rdi, rdi
    shl rdi, 0
    add rsi, rdi

    mov sil, byte [rsi]
    MAKE_CHAR(rax, sil)

    leave
    ret

string_set:
    push rbp
    mov rbp, rsp

    mov rsi, PVAR(0) 
    STRING_ELEMENTS rsi, rsi
    mov rdi, PVAR(1)
    INT_VAL rdi, rdi
    shl rdi, 0
    add rsi, rdi

    mov rax, PVAR(2)
    CHAR_VAL rax, rax
    mov byte [rsi], al
    mov rax, SOB_VOID_ADDRESS

    leave
    ret

make_string:
    push rbp
    mov rbp, rsp

    
    mov rsi, PVAR(0)
    INT_VAL rsi, rsi
    mov rdi, PVAR(1)
    CHAR_VAL rdi, rdi
    and rdi, 255

    MAKE_STRING rax, rsi, dil

    leave
    ret

symbol_to_string:
    push rbp
    mov rbp, rsp

    
    mov rsi, PVAR(0)
    SYMBOL_VAL rsi, rsi
    
    STRING_LENGTH rcx, rsi
    STRING_ELEMENTS rdi, rsi

    push rcx
    push rdi

    mov dil, byte [rdi]
    MAKE_CHAR(rax, dil)
    push rax
    MAKE_INT(rax, rcx)
    push rax
    push 2
    push SOB_NIL_ADDRESS
    call make_string
    add rsp, 4*8

    STRING_ELEMENTS rsi, rax

    pop rdi
    pop rcx

    cmp rcx, 0
    je .end
	
.loop:
    lea r8, [rdi+rcx]
    lea r9, [rsi+rcx]

    mov bl, byte [r8]
    mov byte [r9], bl
    
    loop .loop
.end:

    leave
    ret

char_to_integer:
    push rbp
    mov rbp, rsp

    
    mov rsi, PVAR(0)
    CHAR_VAL rsi, rsi
    and rsi, 255
    MAKE_INT(rax, rsi)

    leave
    ret

integer_to_char:
    push rbp
    mov rbp, rsp

    
    mov rsi, PVAR(0)
    INT_VAL rsi, rsi
    and rsi, 255
    MAKE_CHAR(rax, sil)

    leave
    ret

is_eq:
    push rbp
    mov rbp, rsp

    
    mov rsi, PVAR(0)
    mov rdi, PVAR(1)
    cmp rsi, rdi
    je .true
    mov rax, SOB_FALSE_ADDRESS
    jmp .return

.true:
    mov rax, SOB_TRUE_ADDRESS

.return:
    leave
    ret

bin_add:
    push rbp
    mov rbp, rsp

    mov r8, 0

    mov rsi, PVAR(0)
    push rsi
    push 1
    push SOB_NIL_ADDRESS
    call is_float
    add rsp, 3*WORD_SIZE 


    cmp rax, SOB_TRUE_ADDRESS
    je .test_next
    or r8, 1

.test_next:

    mov rsi, PVAR(1)
    push rsi
    push 1
    push SOB_NIL_ADDRESS
    call is_float
    add rsp, 3*WORD_SIZE 


    cmp rax, SOB_TRUE_ADDRESS
    je .load_numbers
    or r8, 2

.load_numbers:
    push r8

    shr r8, 1
    jc .first_arg_int
    mov rsi, PVAR(0)
    FLOAT_VAL rsi, rsi 
    movq xmm0, rsi
    jmp .load_next_float

.first_arg_int:
    mov rsi, PVAR(0)
    INT_VAL rsi, rsi
    cvtsi2sd xmm0, rsi

.load_next_float:
    shr r8, 1
    jc .second_arg_int
    mov rsi, PVAR(1)
    FLOAT_VAL rsi, rsi
    movq xmm1, rsi
    jmp .perform_float_op

.second_arg_int:
    mov rsi, PVAR(1)
    INT_VAL rsi, rsi
    cvtsi2sd xmm1, rsi

.perform_float_op:
    addsd xmm0, xmm1

    pop r8
    cmp r8, 3
    jne .return_float

    cvttsd2si rsi, xmm0
    MAKE_INT(rax, rsi)
    jmp .return

.return_float:
    movq rsi, xmm0
    MAKE_FLOAT(rax, rsi)

.return:

    leave
    ret

bin_mul:
    push rbp
    mov rbp, rsp

    mov r8, 0

    mov rsi, PVAR(0)
    push rsi
    push 1
    push SOB_NIL_ADDRESS
    call is_float
    add rsp, 3*WORD_SIZE 


    cmp rax, SOB_TRUE_ADDRESS
    je .test_next
    or r8, 1

.test_next:

    mov rsi, PVAR(1)
    push rsi
    push 1
    push SOB_NIL_ADDRESS
    call is_float
    add rsp, 3*WORD_SIZE 


    cmp rax, SOB_TRUE_ADDRESS
    je .load_numbers
    or r8, 2

.load_numbers:
    push r8

    shr r8, 1
    jc .first_arg_int
    mov rsi, PVAR(0)
    FLOAT_VAL rsi, rsi 
    movq xmm0, rsi
    jmp .load_next_float

.first_arg_int:
    mov rsi, PVAR(0)
    INT_VAL rsi, rsi
    cvtsi2sd xmm0, rsi

.load_next_float:
    shr r8, 1
    jc .second_arg_int
    mov rsi, PVAR(1)
    FLOAT_VAL rsi, rsi
    movq xmm1, rsi
    jmp .perform_float_op

.second_arg_int:
    mov rsi, PVAR(1)
    INT_VAL rsi, rsi
    cvtsi2sd xmm1, rsi

.perform_float_op:
    mulsd xmm0, xmm1

    pop r8
    cmp r8, 3
    jne .return_float

    cvttsd2si rsi, xmm0
    MAKE_INT(rax, rsi)
    jmp .return

.return_float:
    movq rsi, xmm0
    MAKE_FLOAT(rax, rsi)

.return:

    leave
    ret

bin_sub:
    push rbp
    mov rbp, rsp

    mov r8, 0

    mov rsi, PVAR(0)
    push rsi
    push 1
    push SOB_NIL_ADDRESS
    call is_float
    add rsp, 3*WORD_SIZE 


    cmp rax, SOB_TRUE_ADDRESS
    je .test_next
    or r8, 1

.test_next:

    mov rsi, PVAR(1)
    push rsi
    push 1
    push SOB_NIL_ADDRESS
    call is_float
    add rsp, 3*WORD_SIZE 


    cmp rax, SOB_TRUE_ADDRESS
    je .load_numbers
    or r8, 2

.load_numbers:
    push r8

    shr r8, 1
    jc .first_arg_int
    mov rsi, PVAR(0)
    FLOAT_VAL rsi, rsi 
    movq xmm0, rsi
    jmp .load_next_float

.first_arg_int:
    mov rsi, PVAR(0)
    INT_VAL rsi, rsi
    cvtsi2sd xmm0, rsi

.load_next_float:
    shr r8, 1
    jc .second_arg_int
    mov rsi, PVAR(1)
    FLOAT_VAL rsi, rsi
    movq xmm1, rsi
    jmp .perform_float_op

.second_arg_int:
    mov rsi, PVAR(1)
    INT_VAL rsi, rsi
    cvtsi2sd xmm1, rsi

.perform_float_op:
    subsd xmm0, xmm1

    pop r8
    cmp r8, 3
    jne .return_float

    cvttsd2si rsi, xmm0
    MAKE_INT(rax, rsi)
    jmp .return

.return_float:
    movq rsi, xmm0
    MAKE_FLOAT(rax, rsi)

.return:

    leave
    ret

bin_div:
    push rbp
    mov rbp, rsp

    mov r8, 0

    mov rsi, PVAR(0)
    push rsi
    push 1
    push SOB_NIL_ADDRESS
    call is_float
    add rsp, 3*WORD_SIZE 


    cmp rax, SOB_TRUE_ADDRESS
    je .test_next
    or r8, 1

.test_next:

    mov rsi, PVAR(1)
    push rsi
    push 1
    push SOB_NIL_ADDRESS
    call is_float
    add rsp, 3*WORD_SIZE 


    cmp rax, SOB_TRUE_ADDRESS
    je .load_numbers
    or r8, 2

.load_numbers:
    push r8

    shr r8, 1
    jc .first_arg_int
    mov rsi, PVAR(0)
    FLOAT_VAL rsi, rsi 
    movq xmm0, rsi
    jmp .load_next_float

.first_arg_int:
    mov rsi, PVAR(0)
    INT_VAL rsi, rsi
    cvtsi2sd xmm0, rsi

.load_next_float:
    shr r8, 1
    jc .second_arg_int
    mov rsi, PVAR(1)
    FLOAT_VAL rsi, rsi
    movq xmm1, rsi
    jmp .perform_float_op

.second_arg_int:
    mov rsi, PVAR(1)
    INT_VAL rsi, rsi
    cvtsi2sd xmm1, rsi

.perform_float_op:
    divsd xmm0, xmm1

    pop r8
    cmp r8, 3
    jne .return_float

    cvttsd2si rsi, xmm0
    MAKE_INT(rax, rsi)
    jmp .return

.return_float:
    movq rsi, xmm0
    MAKE_FLOAT(rax, rsi)

.return:

    leave
    ret

bin_lt:
    push rbp
    mov rbp, rsp

    mov r8, 0

    mov rsi, PVAR(0)
    push rsi
    push 1
    push SOB_NIL_ADDRESS
    call is_float
    add rsp, 3*WORD_SIZE 


    cmp rax, SOB_TRUE_ADDRESS
    je .test_next
    or r8, 1

.test_next:

    mov rsi, PVAR(1)
    push rsi
    push 1
    push SOB_NIL_ADDRESS
    call is_float
    add rsp, 3*WORD_SIZE 


    cmp rax, SOB_TRUE_ADDRESS
    je .load_numbers
    or r8, 2

.load_numbers:
    push r8

    shr r8, 1
    jc .first_arg_int
    mov rsi, PVAR(0)
    FLOAT_VAL rsi, rsi 
    movq xmm0, rsi
    jmp .load_next_float

.first_arg_int:
    mov rsi, PVAR(0)
    INT_VAL rsi, rsi
    cvtsi2sd xmm0, rsi

.load_next_float:
    shr r8, 1
    jc .second_arg_int
    mov rsi, PVAR(1)
    FLOAT_VAL rsi, rsi
    movq xmm1, rsi
    jmp .perform_float_op

.second_arg_int:
    mov rsi, PVAR(1)
    INT_VAL rsi, rsi
    cvtsi2sd xmm1, rsi

.perform_float_op:
    cmpltsd xmm0, xmm1

    pop r8
    cmp r8, 3
    jne .return_float

    cvttsd2si rsi, xmm0
    MAKE_INT(rax, rsi)
    jmp .return

.return_float:
    movq rsi, xmm0
    MAKE_FLOAT(rax, rsi)

.return:

    INT_VAL rsi, rax
    cmp rsi, 0
    je .return_false
    mov rax, SOB_TRUE_ADDRESS
    jmp .final_return

.return_false:
    mov rax, SOB_FALSE_ADDRESS

.final_return:


    leave
    ret

bin_equ:
    push rbp
    mov rbp, rsp

    mov r8, 0

    mov rsi, PVAR(0)
    push rsi
    push 1
    push SOB_NIL_ADDRESS
    call is_float
    add rsp, 3*WORD_SIZE 


    cmp rax, SOB_TRUE_ADDRESS
    je .test_next
    or r8, 1

.test_next:

    mov rsi, PVAR(1)
    push rsi
    push 1
    push SOB_NIL_ADDRESS
    call is_float
    add rsp, 3*WORD_SIZE 


    cmp rax, SOB_TRUE_ADDRESS
    je .load_numbers
    or r8, 2

.load_numbers:
    push r8

    shr r8, 1
    jc .first_arg_int
    mov rsi, PVAR(0)
    FLOAT_VAL rsi, rsi 
    movq xmm0, rsi
    jmp .load_next_float

.first_arg_int:
    mov rsi, PVAR(0)
    INT_VAL rsi, rsi
    cvtsi2sd xmm0, rsi

.load_next_float:
    shr r8, 1
    jc .second_arg_int
    mov rsi, PVAR(1)
    FLOAT_VAL rsi, rsi
    movq xmm1, rsi
    jmp .perform_float_op

.second_arg_int:
    mov rsi, PVAR(1)
    INT_VAL rsi, rsi
    cvtsi2sd xmm1, rsi

.perform_float_op:
    cmpeqsd xmm0, xmm1

    pop r8
    cmp r8, 3
    jne .return_float

    cvttsd2si rsi, xmm0
    MAKE_INT(rax, rsi)
    jmp .return

.return_float:
    movq rsi, xmm0
    MAKE_FLOAT(rax, rsi)

.return:

    INT_VAL rsi, rax
    cmp rsi, 0
    je .return_false
    mov rax, SOB_TRUE_ADDRESS
    jmp .final_return

.return_false:
    mov rax, SOB_FALSE_ADDRESS

.final_return:


    leave
    ret


