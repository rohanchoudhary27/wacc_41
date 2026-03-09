.text
.global main
f_math.abs:
	stp fp, lr, [sp, #-16]!
	mov fp, sp
	stp x19, x20, [sp, #-16]!
	stp x21, x22, [sp, #-16]!
	str x23, [sp, #-16]!
.L_f_math.abs_body:
	mov w19, w0
	movz w16, #0
	mov w20, w16
	cmp w19, w20
	cset w21, lt
	cmp w21, #0
	beq .L_else_0
	movz w16, #0
	mov w22, w16
	subs w23, w22, w19
	bvs _errOverflow
	mov w0, w23
	b .L_f_math.abs_epilogue
	b .L_end_1
.L_else_0:
	mov w0, w19
	b .L_f_math.abs_epilogue
.L_end_1:
.L_f_math.abs_epilogue:
	ldr x23, [sp], #16
	ldp x21, x22, [sp], #16
	ldp x19, x20, [sp], #16
	ldp fp, lr, [sp], #16
	ret
f_math.max:
	stp fp, lr, [sp, #-16]!
	mov fp, sp
	stp x19, x20, [sp, #-16]!
	str x21, [sp, #-16]!
.L_f_math.max_body:
	mov w19, w0
	mov w20, w1
	cmp w19, w20
	cset w21, gt
	cmp w21, #0
	beq .L_else_2
	mov w0, w19
	b .L_f_math.max_epilogue
	b .L_end_3
.L_else_2:
	mov w0, w20
	b .L_f_math.max_epilogue
.L_end_3:
.L_f_math.max_epilogue:
	ldr x21, [sp], #16
	ldp x19, x20, [sp], #16
	ldp fp, lr, [sp], #16
	ret
f_greet.getAnswer:
	stp fp, lr, [sp, #-16]!
	mov fp, sp
	stp x19, x20, [sp, #-16]!
.L_f_greet.getAnswer_body:
	mov w19, w0
	movz w16, #42
	mov w20, w16
	mov w0, w20
	b .L_f_greet.getAnswer_epilogue
.L_f_greet.getAnswer_epilogue:
	ldp x19, x20, [sp], #16
	ldp fp, lr, [sp], #16
	ret
main:
	stp fp, lr, [sp, #-16]!
	mov fp, sp
	stp x19, x20, [sp, #-16]!
	stp x21, x22, [sp, #-16]!
	stp x23, x24, [sp, #-16]!
	str x25, [sp, #-16]!
	movz w16, #0
	mov w19, w16
	movz w16, #3
	mov w20, w16
	subs w21, w19, w20
	bvs _errOverflow
	mov w0, w21
	bl f_math.abs
	mov w22, w0
	mov w23, w22
	movz w0, #0
	bl f_greet.getAnswer
	mov w24, w0
	mov w25, w24
	mov w0, w23
	bl _printi
	bl _println
	mov w0, w25
	bl _printi
	bl _println
	movz w0, #0
	ldr x25, [sp], #16
	ldp x23, x24, [sp], #16
	ldp x21, x22, [sp], #16
	ldp x19, x20, [sp], #16
	ldp fp, lr, [sp], #16
	ret


// RUNTIME LIBRARY:

// prints an integer value to stdout
// length of .L._printi_str0
    .word 2
.L._printi_str0:
    .asciz "%d"
    .align 4
_printi:
    // push {lr}
    stp lr, xzr, [sp, #-16]!
    mov x1, x0
    adr x0, .L._printi_str0
    bl printf
    mov x0, #0
    bl fflush
    // pop {lr}
    ldp lr, xzr, [sp], #16
    ret

// prints a string value to stdout
// length of .L._prints_str0
    .word 4
.L._prints_str0:
    .asciz "%.*s"
    .align 4
_prints:
    // push {lr}
    stp lr, xzr, [sp, #-16]!
    mov x2, x0
    ldur w1, [x0, #-4]
    adr x0, .L._prints_str0
    bl printf
    mov x0, #0
    bl fflush
    // pop {lr}
    ldp lr, xzr, [sp], #16
    ret

// integer overflow or underflow error handler
// length of .L._errOverflow_str0
    .word 52
.L._errOverflow_str0:
    .asciz "fatal error: integer overflow or underflow occurred\n"
    .align 4
_errOverflow:
    adr x0, .L._errOverflow_str0
    bl _prints
    mov w0, #255
    bl exit

// prints a new line to stdout
// length of .L._println_str0
     .word 0
.L._println_str0:
    .asciz ""
    .align 4
_println:
     // push {lr}
     stp lr, xzr, [sp, #-16]!
     adr x0, .L._println_str0
     bl puts
     mov x0, #0
     bl fflush
     // pop {lr}
     ldp lr, xzr, [sp], #16
     ret
